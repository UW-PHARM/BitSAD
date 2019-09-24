package com.github.uwpharm

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform._
import scala.reflect.runtime.universe._
import scala.collection.immutable._
import util.matching._

import java.io._
import scala.io.StdIn

import bitstream.compiler._
import bitstream.compiler.NodeHandlers._
import bitstream.compiler.TypeIdentifiers._
import bitstream.compiler.eval._

class BitSADPlugin(val global: Global) extends Plugin {

  import global._

  val name = "bitsad-plugin"
  val description = "Bitstream DSL Verilog Generator Plugin"
  val components = List[PluginComponent](ImporterComponent,
                                         ModuleComponent,
                                         ConstantReplaceComponent,
                                         OptimizerComponent,
                                         HDLComponent)

  def stripParamKeyword(tree: Tree) = tree.toString.stripPrefix("params.").trim

  def checkConstant(x: String): Boolean = x matches """[\d\.\-]+E?[\d\-]*"""

  // Evaluate an AST that is assumed to represent a constant expression
  def reduceConstantExpr(tree: Tree) : Double = Eval[Double](q"$tree.toDouble".toString)

  var importsMap = Map[String, ImportRecord]()
  var topLevelFile = ""

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    for (option <- options) option match {
      case s if s contains "top:" => topLevelFile = s.stripPrefix("top:")
      case _ => error(s"Unknown plugin option: $option")
    }
  }

  private object ImporterComponent extends PluginComponent {

    val global: BitSADPlugin.this.global.type = BitSADPlugin.this.global
    val runsAfter = List("parser")
    // override val runsRightAfter = Some("parser")
    // override val runsBefore = List("namer")
    val phaseName = s"${BitSADPlugin.this.name}-importer"

    class ImporterPhase(prev: Phase) extends StdPhase(prev) {

      def apply(unit: CompilationUnit) {
        var record = ImportRecord(unit.source.toString)
        for (node <- unit.body) node match {
          case Import(expr, selectors) => for (selector <- selectors) selector match {
            case ImportSelector(name, _, rename, _) if name.toString == "Module" =>
              record.addRenameRule(expr.toString, rename.toString)
            case _ =>
          }
          case _ =>
        }

        record.mkInvRenameMap()
        importsMap += (record.filename -> record)
      }

    }

    def newPhase(_prev: Phase) = new ImporterPhase(_prev)

  }

  private object ConstantReplaceComponent extends PluginComponent with TypingTransformers {

    val global: BitSADPlugin.this.global.type = BitSADPlugin.this.global
    val runsAfter = List(ImporterComponent.phaseName)
    // override val runsRightAfter = Some("parser")
    // override val runsBefore = List("namer")
    val phaseName = s"${BitSADPlugin.this.name}-constrepl"
    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      def apply(unit: CompilationUnit) {
        importsMap(unit.source.toString).setDefaultParams(getFinalVals(unit.body))
        unit.body = new ConstantReplaceTransformer(unit).transform(unit.body)
      }
    }

    def getFinalVals(tree: Tree): Map[String, Double] = {
      var cMap = Map[String, Double]()
      for (node <- tree) node match {
        case ModuleDef(_, name, body) if name.toString == "DefaultParams" =>
          for (statement <- body.children.drop(3)) statement match {
            case ValDef(mods, opd, tpt, rhs) => cMap = cMap ++ getSubParams(opd.toString.trim, rhs)
            case DefDef(Modifiers(flags, _, _), _, _, _, _, _) if (flags & Flag.STABLE) != 0 =>
              // we assume that stable accessors are related to ValDefs with proper assignments
            case _ =>
              warning(s"[GET PARAMS] $statement\n==> is not a ValDef. DefaultParams should only contain value definitions.")
          }
        case _ =>
      }

      cMap
    }

    def getSubParams(prefix: String, tree: Tree): Map[String, Double] = {
      var cMap = Map[String, Double]()
      tree match {
        case Block(List(ClassDef(_, name, _, impl)), _) => for (statement <- impl.body.tail) statement match {
          case ValDef(_, opd, _, rhs) => cMap = cMap ++ getSubParams(s"${prefix}_$opd".trim, rhs)
          case DefDef(Modifiers(flags, _, _), _, _, _, _, _) if (flags & Flag.STABLE) != 0 =>
            // we assume that stable accessors are related to ValDefs with proper assignments
          case _ =>
            warning(s"[GET PARAMS] $tree\n==> is not an instance of a parameter class nor a constant. Ignoring.")
        }
        case _ if checkConstant(tree.toString) => cMap += (prefix -> tree.toString.toDouble)
        case _ =>
          warning(s"[GET PARAMS] $tree\n==> is not an instance of a parameter class nor a constant. Ignoring.")
      }
      cMap
    }

    class ConstantReplaceTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

      val params = importsMap(unit.source.toString).params
      var inModule = false
      var inLoop = false

      override def transform(tree: Tree) = tree match {
        case ClassDef(_, classname, _, impl) if classname.toString == "Module" => {
          inModule = true
          val outTree = super.transform(tree)
          inModule = false
          outTree
        }
        case DefDef(_, name, _, _, _, body) if inModule && name.toString == "loop" => {
          inLoop = true
          val outTree = super.transform(tree)
          inLoop = false
          outTree
        }
        case _ if inModule && inLoop => {
          val name = stripParamKeyword(tree)
          if (unit.source.toString == topLevelFile) {
            if (params contains name) super.transform(q"${params(name)}")
            else super.transform(tree)
          } else super.transform(tree)
        }
        case _ => super.transform(tree)
      }

    }

    def newTransformer(unit: CompilationUnit) = new ConstantReplaceTransformer(unit)

  }

  private object ModuleComponent extends PluginComponent {

    val global: BitSADPlugin.this.global.type = BitSADPlugin.this.global
    val runsAfter = List(ConstantReplaceComponent.phaseName)
    // override val runsRightAfter = Some("parser")
    override val runsBefore = List("namer")
    val phaseName = s"${BitSADPlugin.this.name}-moduleparser"

    class ModuleParsePhase(prev: Phase) extends StdPhase(prev) {

      def apply(unit: CompilationUnit) = {
        val defaultParams = importsMap(unit.source.toString).params
        for (node <- unit.body) node match {
          case ClassDef(_, classname, _, impl) if classname.toString == "Module" =>
            for (subNode <- impl.body) subNode match {
              case ValDef(_, name, _, rhs) if name.toString.trim == "outputList" => rhs match {
                case Apply(_, list) => for (output <- list) {
                  val q"(${name: String}, ${r: Int}, ${c: Int})" = output
                  importsMap(unit.source.toString).addOutput(name)
                  importsMap(unit.source.toString).addOutputSize((r, c))
                }
                case _ =>
              }
              case ValDef(_, name, tpt, rhs) => {
                val (moduleTopName, paramName) = rhs match {
                  case Apply(module, List(src)) => (module.toString, stripParamKeyword(src))
                  case _ => ("", "")
                }
                if (importsMap(unit.source.toString).isModule(moduleTopName)) {
                  val params = defaultParams.keys.filter(k => k.startsWith(paramName)).toList
                  importsMap(unit.source.toString).addModuleRule(name.toString, moduleTopName, params)
                }
              }
              case _ =>
            }
          case _ =>
        }
      }

    }

    def newPhase(_prev: Phase) = new ModuleParsePhase(_prev)

  }

  private object OptimizerComponent extends PluginComponent with TypingTransformers {

    val global: BitSADPlugin.this.global.type = BitSADPlugin.this.global
    val runsAfter = List(ConstantReplaceComponent.phaseName)
    // override val runsRightAfter = Some("parser")
    override val runsBefore = List("namer")
    val phaseName = s"${BitSADPlugin.this.name}-optimize"
    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      def apply(unit: CompilationUnit) {
        unit.body = new OptimizationTransformer(unit).transform(unit.body)
      }
    }

    class OptimizationTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

      override def transform(tree: Tree) = tree match {
        case ClassDef(classmods, classname, classtparams, impl) if classname.toString == "Module" => {
          var implStatements: List[Tree] = List()
          for (node <- impl.body) node match {
            case DefDef(mods, name, tparams, vparamss, tpt, body) if name.toString == "loop" => {
              var statements: List[Tree] = List()
              for (statement <- body.children.dropRight(1)) statement match {
                case Assign(opd, rhs) => {
                  val optimizedRHS = optimizeStatement(rhs)
                  statements = statements ++ List(treeCopy.Assign(statement, opd, optimizedRHS))
                }
                case ValDef(mods, opd, tpt, rhs) => {
                  val optimizedRHS = optimizeStatement(rhs)
                  statements = statements ++
                    List(treeCopy.ValDef(statement, mods, opd, tpt, optimizedRHS))
                }
                case Apply(Select(src1, op), List(src2)) if op.toString == "push" => {
                  val optimizedSrc2 = optimizeStatement(src2)
                  statements = statements ++
                    List(treeCopy.Apply(statement, Select(src1, op), List(optimizedSrc2)))
                }
                case _ => statements = statements ++ List(statement)
              }

              val newBody = treeCopy.Block(body, statements, body.children.last)
              implStatements = implStatements ++
                List(treeCopy.DefDef(node, mods, name, tparams, vparamss, tpt, newBody))
            }
            case _ => implStatements = implStatements ++ List(node)
          }
          val newImpl = treeCopy.Template(impl, impl.parents, impl.self, implStatements)
          treeCopy.ClassDef(tree, classmods, classname, classtparams, newImpl)
        }
        case _ => super.transform(tree)
      }

      def optimizeStatement(statement: Tree): Tree = {
        if (statement.children.length == 0) statement
        else statement match {
          case Apply(Apply(Select(src1, TermName(op)), List(src2)), List(src3)) => {
            var optimizedSrc1 = optimizeStatement(src1)
            var optimizedSrc2 = optimizeStatement(src2)
            var newOp: BitSADPlugin.this.global.TermName = TermName(op)

            var optimizedNode =
              atPos(statement.pos.focus)(q"$optimizedSrc1.$newOp($optimizedSrc2)($src3)")
            optimizeStatementHelper(optimizedNode)
          }
          case Apply(Select(src1, TermName(op)), List(src2)) => {
            var optimizedSrc1 = optimizeStatement(src1)
            var optimizedSrc2 = optimizeStatement(src2)
            var newOp: BitSADPlugin.this.global.TermName = TermName(op)

            var optimizedNode =
              atPos(statement.pos.focus)(q"$optimizedSrc1.$newOp($optimizedSrc2)")
            optimizeStatementHelper(optimizedNode)
          }
          case Select(src1, TermName(op)) => {
            var optimizedSrc1 = optimizeStatement(src1)
            var newOp: BitSADPlugin.this.global.TermName = TermName(op)

            var optimizedNode = atPos(statement.pos.focus)(q"$optimizedSrc1.$newOp")
            optimizeStatementHelper(optimizedNode)
          }
          case _ => statement
        }
      }

      def optimizeStatementHelper(statement: Tree): Tree = {
        // propagate constant via distributive multiplication
        var optimizedStatement = propagateConstant(statement)

        // println(s"[PROP CONST] ${optimizedStatement.toString}")

        // collect like terms
        optimizedStatement = collectLikeTerms(optimizedStatement)

        // println(s"[COLL TERMS] ${optimizedStatement.toString}")

        atPos(statement.pos.focus)(optimizedStatement)
      }

      def propagateConstant(tree: Tree): Tree = tree match {
        case Apply(Select(src1, op), List(src2)) if op == nme.MUL => {
          if (checkConstant(src1.toString)) {
            // distribute term by term to form new tree
            if (src2.children.length == 0) tree
            else distributeMultConst(src2, src1)
          } else if (checkConstant(src2.toString)) {
            // distribute term by term to form new tree
            if (src1.children.length == 0) tree
            else distributeMultConst(src1, src2)
          } else tree
        }
        case _ => tree
      }

      def distributeMultConst(tree: Tree, c: Tree): Tree = {
        // println(s"[DISTR] $c * ($tree)")

        var nodes: List[BitSADPlugin.this.global.Tree] = List()
        var ops: List[BitSADPlugin.this.global.TermName] = List()

        var outputTree: BitSADPlugin.this.global.Tree = EmptyTree
        if (isTreeAdditive(tree)) {
          tree.children.dropRight(1).foreach((node: Tree) => node match {
            case Select(src, op) if op == nme.ADD || op == nme.SUB => {
              var newNode =
                if (src.children.length == 0) simplifyNestedConstants(c, src)
                else distributeMultConst(src, c)

              nodes = nodes ++ List(newNode)
              ops = ops ++ List(TermName(op.toString))
            }
            case _ =>
          })

          outputTree = simplifyNestedConstants(c, tree.children.last)
          val treeBuilder = (x: Tuple2[Tree, TermName], ast: Tree) =>
            q"${x._1}.${x._2}($ast)"
          outputTree = nodes.zip(ops).foldRight(outputTree)(treeBuilder)
        } else outputTree = q"$c * $tree"

        outputTree
      }

      def simplifyNestedConstants(c: Tree, tree: Tree): Tree = {
        var node = q"$c * $tree"
        tree match {
          case Apply(Select(src1, op), List(src2)) if checkConstant(src1.toString) =>
            var coefficient = q"${reduceConstantExpr(q"$c * $src1")}"
            node = q"$coefficient * $src2"
          case _ => {
            if (checkConstant(tree.toString) && tree.children.length == 0)
              node = q"${reduceConstantExpr(q"$c * $tree")}"
          }
        }

        // println(s"[CONST SIMPL] $c   $tree")
        // println(s"[CONST SIMPL]  ==>  ${node.toString}")
        node
      }

      def collectLikeTerms(tree: Tree): Tree = {
        var coefficients: List[BitSADPlugin.this.global.Tree] = List()
        var nodes: List[BitSADPlugin.this.global.Tree] = List()
        var ops: List[BitSADPlugin.this.global.TermName] = List()

        if (isTreeAdditive(tree)) {
          getLikeTerms(tree) match {
            case (c, x, o) => {
              // println(s"[COEFF] $c")
              // println(s"[NODES] $x")
              // println(s"[OPS]   $o")
              coefficients = c
              nodes = x
              ops = o
            }
          }

          var termMap: Map[String, Tuple2[Tree, Tree]] = Map()
          val collector = (x:Tuple3[Tree, Tree, TermName]) => {
            if (termMap.exists(_._1 == x._2.toString)) {
              var newCoefficient =
                q"${reduceConstantExpr(q"${termMap(x._2.toString)._2}.${x._3}(${x._1})")}"
              termMap += (x._2.toString -> (x._2, newCoefficient))
            } else {
              var newCoefficient =
                if (x._3 == nme.SUB && checkConstant(x._1.toString))
                  q"${reduceConstantExpr(q"-${x._1}")}"
                else x._1
              termMap += (x._2.toString -> (x._2, newCoefficient))
            }
          }
          (coefficients, nodes, ops).zipped.toList.foreach(collector)

          // println(termMap)

          var outputTree: BitSADPlugin.this.global.Tree = EmptyTree
          val builder = (x: String, ast: Tree) => {
            var newTree: BitSADPlugin.this.global.Tree = EmptyTree
            var coefficient: BitSADPlugin.this.global.Tree = EmptyTree
            var node: BitSADPlugin.this.global.Tree = EmptyTree
            termMap(x) match {
              case (n, c) => {
                coefficient = c
                node = n
              }
            }

            if (!node.isEmpty) {
              if (coefficient.toString == "1" || coefficient.toString == "1.0")
                newTree = q"$ast + $node"
              else if (coefficient.toString == "-1" || coefficient.toString == "-1.0")
                newTree = q"$ast - $node"
              else newTree = q"$ast + $coefficient * $node"
            }

            newTree
          }
          var uniqueNodes = nodes.map(_.toString).distinct
          // println(uniqueNodes)
          var (node, coefficient) = termMap(uniqueNodes.last)
          if (!node.isEmpty) {
            if (coefficient.toString == "1" || coefficient.toString == "1.0")
              outputTree = node
            else if (coefficient.toString == "-1" || coefficient.toString == "-1.0")
              outputTree = q"-$node"
            else outputTree = q"$coefficient * $node"
          }
          outputTree = uniqueNodes.dropRight(1).foldRight(outputTree)(builder)

          outputTree
        } else tree
      }

      def getLikeTerms(tree: Tree): (List[Tree], List[Tree], List[TermName]) = {
        var coefficients: List[Tree] = List()
        var nodes: List[Tree] = List()
        var ops: List[TermName] = List()

        tree match {
          case Apply(Select(src1, op), List(src2)) if op == nme.ADD || op == nme.SUB => {
            src1 match {
              case Apply(Select(a, subOp), List(b)) if subOp == nme.ADD || subOp == nme.SUB =>
                getLikeTerms(src1) match {
                  case (subCoefficients, subNodes, subOps) => {
                    coefficients = subCoefficients ++ coefficients
                    nodes = subNodes ++ nodes
                    ops = subOps ++ ops
                  }
                }
              case Apply(Select(c, subOp), List(x)) if subOp == nme.MUL => {
                if (checkConstant(c.toString)) {
                  coefficients = List(c) ++ coefficients
                  nodes = List(x) ++ nodes
                  ops = List(TermName("+")) ++ ops
                } else if (checkConstant(x.toString)) {
                  coefficients = List(x) ++ coefficients
                  nodes = List(c) ++ nodes
                  ops = List(TermName("+")) ++ ops
                } else {
                  coefficients = List(q"1") ++ coefficients
                  nodes = List(src1) ++ nodes
                  ops = List(TermName("+")) ++ ops
                }
              }
              case _ => {
                if (checkConstant(src1.toString)) {
                  coefficients = List(src1) ++ coefficients
                  nodes = List(EmptyTree) ++ nodes
                  ops = List(TermName("+")) ++ ops
                } else {
                  coefficients = List(q"1") ++ coefficients
                  nodes = List(src1) ++ nodes
                  ops = List(TermName("+")) ++ ops
                }
              }
            }

            src2 match {
              case Apply(Select(a, subOp), List(b)) if subOp == nme.ADD || subOp == nme.SUB =>
                getLikeTerms(src2) match {
                  case (subCoefficients, subNodes, subOps) => {
                    coefficients = subCoefficients ++ coefficients
                    nodes = subNodes ++ nodes
                    ops = subOps ++ ops
                  }
                }
              case Apply(Select(c, subOp), List(x)) if subOp == nme.MUL => {
                if (checkConstant(c.toString)) {
                  coefficients = List(c) ++ coefficients
                  nodes = List(x) ++ nodes
                } else if (checkConstant(x.toString)) {
                  coefficients = List(x) ++ coefficients
                  nodes = List(c) ++ nodes
                } else {
                  coefficients = List(q"1") ++ coefficients
                  nodes = List(src2) ++ nodes
                  ops = List(TermName("+")) ++ ops
                }
                ops = List(TermName(op.toString)) ++ ops
              }
              case _ => {
                if (checkConstant(src2.toString)) {
                  coefficients = List(src2) ++ coefficients
                  nodes = List(EmptyTree) ++ nodes
                  ops = List(TermName(op.toString)) ++ ops
                } else {
                  coefficients = List(q"1") ++ coefficients
                  nodes = List(src2) ++ nodes
                  ops = List(TermName("+")) ++ ops
                }
              }
            }
          }
          case _ =>
        }

        (coefficients, nodes, ops)
      }

      def isTreeAdditive(tree: Tree): Boolean =
        (tree.children.length > 1) && tree.children.dropRight(1).forall(
          (node: Tree) => node match {
            case Select(src1, op) if op == nme.ADD || op == nme.SUB => true
            case _ => false
          })

    }

    def newTransformer(unit: CompilationUnit) = new OptimizationTransformer(unit)

  }

  private object HDLComponent extends PluginComponent {

    val global: BitSADPlugin.this.global.type = BitSADPlugin.this.global
    val runsAfter = List[String]("refchecks")
    val phaseName = s"${BitSADPlugin.this.name}-hdlgeneration"
    def newPhase(_prev: Phase) = new HDLGenPhase(_prev)

    class HDLGenPhase(prev: Phase) extends StdPhase(prev) {

      override def name = BitSADPlugin.this.name

      // Data structures for storing DFG
      var funcBody            = ""
      var netList             = new Netlist()
      var inputMap            = Map[String, TypeIdentity]()
      var outList             = List[String]()
      var feedbackMode        = false
      var bitWidthList        = List[Tuple2[Int, Int]]()
      var imports             = ImportRecord("")

      // Handlers for generating Verilog code and updating netlist
      // stochastic handlers
      var sAssignHandler       = new SAssignHandler()
      var sAddHandler          = new SAddHandler() // _ + _
      var sSubHandler          = new SSubHandler() // _ - _
      var sMulHandler          = new SMatrixMultiplyHandler() // _ * _
      var sCrossProdHandler    = new SCrossProdHandler() // _.cross(_)
      var sDivHandler          = new SDivHandler() // _ / _
      var sFixedGainDivHandler = new SFixedGainDivHandler() // _ :/ constant
      var sL2NormHandler       = new SL2NormHandler() // Matrix.norm(_)
      var sSqrtHandler         = new SSqrtHandler() // math.sqrt(_)
      var sDecorrelatorHandler = new SDecorrelatorHandler() // decorrelator only used for feedback
      // deterministic handlers
      var dAssignHandler       = new DAssignHandler()
      var dAddHandler          = new DAddHandler()
      var dSubHandler          = new DSubHandler()
      var dMulHandler          = new DMultHandler()
      var dAddFxpHandler       = new DAddFxpHandler()
      var dSubAFxpHandler      = new DSubAFxpHandler()
      var dSubBFxpHandler      = new DSubBFxpHandler()
      var dMulFxpHandler       = new DMultFxpHandler()
      // fxp handlers
      var fAddHandler          = new FxpAddHandler()
      var fSubHandler          = new FxpSubHandler()
      var fMulHandler          = new FxpMultHandler()
      // misc
      var transposeHandler     = new TransposeHandler() // _.T
      var delayBuffHandler     = new DelayBufferHandler() // DelayBuffer.pop/push(_)
      var sdmHandler           = new SDMHandler() // SDM.evaluate(_)

      def resetGlobals(): Unit = {
        funcBody             = ""
        netList              = new Netlist()
        inputMap             = Map[String, TypeIdentity]()
        outList              = List[String]()
        feedbackMode         = false
        bitWidthList         = List[Tuple2[Int, Int]]()
        imports              = ImportRecord("")
        sAssignHandler       = new SAssignHandler()
        sAddHandler          = new SAddHandler()
        sSubHandler          = new SSubHandler()
        sMulHandler          = new SMatrixMultiplyHandler()
        sDivHandler          = new SDivHandler()
        sFixedGainDivHandler = new SFixedGainDivHandler()
        sL2NormHandler       = new SL2NormHandler()
        sSqrtHandler         = new SSqrtHandler()
        sDecorrelatorHandler = new SDecorrelatorHandler()
        dAssignHandler       = new DAssignHandler()
        dAddHandler          = new DAddHandler()
        dSubHandler          = new DSubHandler()
        dMulHandler          = new DMultHandler()
        dAddFxpHandler       = new DAddFxpHandler()
        dSubAFxpHandler      = new DSubAFxpHandler()
        dSubBFxpHandler      = new DSubBFxpHandler()
        dMulFxpHandler       = new DMultFxpHandler()
        fAddHandler          = new FxpAddHandler()
        fSubHandler          = new FxpSubHandler()
        fMulHandler          = new FxpMultHandler()
        transposeHandler     = new TransposeHandler()
        delayBuffHandler     = new DelayBufferHandler()
        sdmHandler           = new SDMHandler()
      }

      def apply(unit: CompilationUnit) {
        println(s"\nGenerating Verilog for ${unit.source.toString}")
        resetGlobals()
        imports = importsMap(unit.source.toString)
        outList = imports.getOutList()
        for (node <- unit.body) node match {
          case ClassDef(_, classname, _, impl) if classname.toString == "Module" => {
            feedbackMode = getFeedbackMode(impl.body)

            for (subNode <- impl.body) subNode match {
              case DefDef(_, name, _, List(vparamss), _, body) if name.toString == "loop" => {
                inputMap = getInputs(vparamss)
                scanRunLoop(body)
              }
              case _ =>
            }

            netList.setOutputs(outList)

            val buffNames = delayBuffHandler.getBufferNames
            val buffDelays = getDelays(buffNames, impl.body)
            delayBuffHandler.setDelay(buffNames, buffDelays)
            funcBody = funcBody + delayBuffHandler.createAll()

            printVerilog(funcBody)
          }
          case _ =>
        }
      }

      def scanRunLoop(runLoop: Tree) : Unit = {
        var statementID = 0

        for(statement <- runLoop.children.dropRight(1)) {
          statement match {
            // predifined opd = rhs
            case Assign(opd, rhs) => scanStatement(rhs, opd.toString, statementID, 0)
            // val/var opd = rhs
            case ValDef(mods, opd, tpt, rhs) => {
              var opdR, opdC = 1
              var opdStr = opd.toString.stripPrefix("_")

              scanStatement(rhs, opdStr, statementID, 0)

              // assume opd is single terminal now
              if ((outList contains opdStr) && (netList contains opdStr) && feedbackMode) {
                getSize(opdStr, SBITSTREAM)
                  match {case(opdNew, row, col) => {opdStr = opdStr; opdR = row; opdC = col}}
                sDecorrelatorHandler.create(List(opdStr), s"${opdStr}_decor", List(Tuple2(opdR, opdC)), netList)
                  match {case(list, str) => {netList = list; funcBody = funcBody + str}}
              }
            }
            case Apply(Select(src1, op), List(src2)) if op.toString == "push" => {
              // Strip away "Module.this" from internal unit names
              var actualSrc1 = detectInternalUnit(src1)

              // Check LHS
              actualSrc1 = detectLHSOp(actualSrc1)

              // Check bit check bit conversion
              actualSrc1 = detectBitConversion(actualSrc1)
              var actualSrc2 = detectBitConversion(src2)

              // Get types
              val types = checkTypes(List(actualSrc1, actualSrc2))

              // Parse operands
              val src1Name = scanStatement(actualSrc1, "", statementID, 1)
              val src2Name = scanStatement(actualSrc2, "", statementID, 1)
              parseOperator(op, src1Name, src2Name, "", types)
            }
            case _ =>
              // println(s"Unknown statement in runLoop(); ID: $statementID; statement: ${statement.toString}")
          }

          statementID += 1
        }
      } // end of func scanRunLoop

      def scanStatement(tree: Tree, dest: String, statementID: Int, levelID: Int): String = {
        var src1Name = ""
        var src2Name = ""

        // println(s"[SCANNING] ${tree.toString}")

        // Set output name
        var output = dest match {
          case "" => s"net${statementID}_${levelID}"
          case "_l_" => s"net${statementID}_${levelID}_l"
          case "_r_" => s"net${statementID}_${levelID}_r"
          case _ => dest
        }

        // Check if tree is terminal node
        if (tree.children.length == 0) levelID match {
          case 0 => {
            // Set names
            val src1 = tree

            // Get types
            val types = checkTypes(List(src1))

            // Get size
            var src1R = 1
            var src1C = 1
            getSize(src1.toString, types(0))
              match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}

            // Check types
            types match {
              case List(SBITSTREAM) =>
                sAssignHandler.create(List(src1Name), output, List((src1R, src1C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(DBITSTREAM) | List(NUMERIC) | List(BIT) =>
                dAssignHandler.create(List(src1Name), output, List((src1R, src1C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
            }
          }
          case _ => output = tree.toString
        // Parse tree if it isn't a terminal node
        } else tree match {
          case Apply(Select(src, op), vsrc) if op.toString.trim == "loop" => {
            // Check LHS
            var actualSrc1 = detectInternalUnit(src)

            // Check bit check bit conversion
            var actualVSrc = vsrc.map(detectBitConversion(_))

            // Get types
            val types = checkTypes(actualVSrc)

            // Get names
            val names = actualVSrc.map(_.toString)

            // Get sizes
            val namesAndSize = for ((name, typeId) <- names.zip(types)) yield getSize(name, typeId)
            var (vSrcNames, sizes) = namesAndSize.unzip(x => (x._1, (x._2, x._3)))

            // Get top level name
            if (imports.checkModule(actualSrc1.toString.trim)) {
              // Get top level and parameters
              val (toplevel, params) = imports.getModule(actualSrc1.toString.trim)

              // Get source file for module
              val sourcename = imports.checkInvRenameRule(toplevel)

              // Get output sizes
              val outSize = importsMap(s"$sourcename.scala").getOutSizeList()
              sizes = sizes ++ outSize

              // Add module instance
              addModuleInstance(actualSrc1.toString, toplevel, params, vSrcNames, output, sizes, netList)
                match {case (list, str) => {netList = list; funcBody = funcBody + str}}
            } else warning(s"Unknown call to module: ${actualSrc1.toString}")
          }
          case Select(src1, op) if op.toString == "toDouble" || op.toString == "toInt" =>
            scanStatement(src1, dest, statementID, levelID)
          case Select(src1, op) => {
            // Strip away "Module.this" from internal unit names
            val actualSrc1 = detectInternalUnit(src1)

            // Get types
            val types = checkTypes(List(actualSrc1))

            // Parse operands
            src1Name = scanStatement(actualSrc1, "_l_", statementID, levelID + 1)
            parseOperator(op, src1Name, src2Name, output, types)
          }
          case Apply(Select(src1, op), List(src2)) => {
            var types: List[TypeIdentity] = List()

            // Fixed gain division => src2 is a constant
            if (op.toString == "$colon$div") {
              // get the actual name, without any types
              // example: types.this.Matrix.MatrixFixedGainDiv(w), only need w
              // dontcare = Ident(bitstream.types.Matrix), TermName("MatrixFixedGainDiv")
              // only applies to case where we haven't already assigned an internal net name
              var actualSrc1 = src1
              for (Apply(dontcare, List(actualSrc)) <- src1) actualSrc1 = actualSrc

              // Get types
              types = checkTypes(List(actualSrc1, src2))

              // Parse operands
              src1Name = scanStatement(actualSrc1, "_l_", statementID, levelID + 1)
              src2Name = math.ceil(reduceConstantExpr(src2)).toInt.toString
            } else {
              // Check LHS
              var actualSrc1 = detectInternalUnit(src1)
              actualSrc1 = detectLHSOp(actualSrc1)

              // Check bit check bit conversion
              var actualSrc2 = src2
              actualSrc1 = detectBitConversion(actualSrc1)
              actualSrc2 = detectBitConversion(src2)

              // Get types
              types = checkTypes(List(actualSrc1, actualSrc2))

              // Parse operands
              if (!isMatrixObject(actualSrc1) && !isSBitstreamObject(actualSrc1))
                src1Name = scanStatement(actualSrc1, "_l_", statementID, levelID + 1)
              src2Name = scanStatement(actualSrc2, "_r_", statementID, levelID + 1)
            }

            parseOperator(op, src1Name, src2Name, output, types)
          }
          case Apply(Apply(Select(src1, op), List(src2)), List(src3)) => {
            // Check LHS
            var actualSrc1 = detectInternalUnit(src1)
            actualSrc1 = detectLHSOp(actualSrc1)

            // Check bit check bit conversion
            var actualSrc2 = src2
            actualSrc1 = detectBitConversion(actualSrc1)
            actualSrc2 = detectBitConversion(src2)

            // Get types
            val types = checkTypes(List(actualSrc1, actualSrc2))

            // Parse operands
            if (!isMatrixObject(actualSrc1) && !isSBitstreamObject(actualSrc1))
              src1Name = scanStatement(actualSrc1, "_l_", statementID, levelID + 1)
            src2Name = scanStatement(actualSrc2, "_r_", statementID, levelID + 1)
            parseOperator(op, src1Name, src2Name, output, types)
            // Ignoring src3 which is implicit arg
          }
          case _ => warning(s"Could match statement structure: $tree")
        }

        output
      }

      def parseOperator(op: Name, src1: String, src2: String, dest: String,
                        types: List[TypeIdentity]): Unit = {
        var src1Name, src2Name = ""
        var src1R, src1C = 1
        var src2R, src2C = 1

        op match {
          case nme.ADD => {
            // Get sizes
            getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
            getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

            // Call handler
            types match {
              case List(SBITSTREAM, SBITSTREAM) =>
                sAddHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(DBITSTREAM, DBITSTREAM) | List(BIT, BIT) =>
                dAddHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(BIT, NUMERIC) | List(DBITSTREAM, NUMERIC) | List(BIT, CONSTANT) | List(DBITSTREAM, CONSTANT) =>
                dAddFxpHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, BIT) | List(NUMERIC, DBITSTREAM) | List(BIT, CONSTANT) | List(DBITSTREAM, CONSTANT) =>
                dAddFxpHandler.create(List(src2Name, src1Name), dest, List((src2R, src2C), (src1R, src1C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, NUMERIC) | List(CONSTANT, NUMERIC) | List(NUMERIC, CONSTANT) =>
                fAddHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case _ => warning("Trying to add types with no associated Verilog module")
            }
          }
          case nme.SUB => {
            // Get sizes
            getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
            getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

            // Call handler
            types match {
              case List(SBITSTREAM, SBITSTREAM) =>
                sSubHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(DBITSTREAM, DBITSTREAM) | List(BIT, BIT) =>
                dSubHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(BIT, NUMERIC) | List(DBITSTREAM, NUMERIC) | List(BIT, CONSTANT) | List(DBITSTREAM, CONSTANT) =>
                dSubAFxpHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, BIT) | List(NUMERIC, DBITSTREAM) | List(BIT, CONSTANT) | List(DBITSTREAM, CONSTANT) =>
                dSubBFxpHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, NUMERIC) | List(CONSTANT, NUMERIC) | List(NUMERIC, CONSTANT) =>
                fSubHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case _ => warning("Trying to sub types with no associated Verilog module")
            }
          }
          case nme.MUL => {
            // Get sizes
            getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
            getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

            // Call handler
            types match {
              case List(SBITSTREAM, SBITSTREAM) =>
                sMulHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(DBITSTREAM, DBITSTREAM) =>
                dMulHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(DBITSTREAM, NUMERIC) | List(BIT, NUMERIC) | List(DBITSTREAM, CONSTANT) | List(BIT, CONSTANT) =>
                dMulFxpHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, DBITSTREAM) | List(NUMERIC, BIT) | List(CONSTANT, DBITSTREAM) | List(CONSTANT, BIT) =>
                dMulFxpHandler.create(List(src2Name, src1Name), dest, List((src2R, src2C), (src1R, src1C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case List(NUMERIC, NUMERIC) | List(CONSTANT, NUMERIC) | List(NUMERIC, CONSTANT) =>
                fMulHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case default => warning("Trying to mult types with no associated Verilog module")
            }
          }
          case nme.DIV => {
            // Get sizes
            getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
            getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

            // Call handler
            types match {
              case List(SBITSTREAM, SBITSTREAM) =>
                sDivHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              case _ => warning("Trying to div types with no associated Verilog module")
            }
          }
          case _ => {
            op.toString match {
              case "norm" => {
                // src1 is the "Matrix" object
                // Get sizes
                getSize(src2, types(0)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

                // Call handler
                types match {
                  case List(SBITSTREAM) =>
                    sL2NormHandler.create(List(src2Name), dest, List((src1R, src1C)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  case _ => warning("Trying to norm type with no associated Verilog module")
                }
              }
              case "sqrt" => {
                // src1 is the "SBitstream" object
                // Get sizes
                getSize(src2, types(0)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

                // Call handler
                types match {
                  case List(SBITSTREAM) =>
                    sSqrtHandler.create(List(src2Name), dest, List((src2R, src2C)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  case _ => warning("Trying to sqrt type with no associated Verilog module")
                }
              }
              case "$colon$div" => {
                // Get sizes
                getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
                src2Name = src2

                // Call handler
                types match {
                  case List(SBITSTREAM, NUMERIC) =>
                    sFixedGainDivHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (1, 1)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  case _ => warning("Trying to fgdiv types with no associated Verilog module")
                }
              }
              case "cross" => {
                // Get sizes
                getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
                getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

                // Call handler
                types match {
                  case List(SBITSTREAM, SBITSTREAM) =>
                    sCrossProdHandler.create(List(src1Name, src2Name), dest, List((src1R, src1C), (src2R, src2C)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  case _ => warning("Trying to cross types with no associated Verilog module")
                }
              }
              case "T" => {
                // Get sizes
                getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}

                // Call handler
                transposeHandler.create(List(src1Name), dest, List((src1R, src1C)), netList)
                  match {case (list, str) => {netList = list; funcBody = funcBody + str}}
              }
              case "pop" => {
                src1Name = src1.toString

                // Add pop to delay handler
                types match {
                  case List(BUFFER, _*) =>
                    netList = delayBuffHandler.addPop(src1Name, dest, (src1R, src1C), netList)
                  case List(DBITSTREAM, _*) => {
                    // Get sizes
                    getSize(src1, types(0)) match {case (name, rows, cols) => {src1Name = name; src1R = rows; src1C = cols}}
                    dAssignHandler.create(List(src1Name), dest, List((src1R, src1C)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  }
                  case _ => warning("Trying to pop types with no associated Verilog module")
                }
              }
              case "push" => {
                src1Name = src1.toString
                // Get sizes
                getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

                // Add pop to delay handler
                types match {
                  case List(BUFFER, NUMERIC) | List(BUFFER, BIT) =>
                    netList = delayBuffHandler.addPush(src1Name, src2Name, (src2R, src2C), netList)
                  case _ => warning("Trying to push types with no associated Verilog module")
                }
              }
              case "evaluate" => {
                // Get sizes
                getSize(src2, types(1)) match {case (name, rows, cols) => {src2Name = name; src2R = rows; src2C = cols}}

                // Call handler
                types match {
                  case List(SDM, NUMERIC) | List(SDM, CONSTANT) =>
                    sdmHandler.create(List(src2Name), dest, List((src2R, src2C)), netList)
                      match {case (list, str) => {netList = list; funcBody = funcBody + str}}
                  case _ => warning("Trying to evaluate types with no associated Verilog module")
                }
              }
              case _ => warning(s"Unknown node in parseOperator(): ${op.toString}")
            }
          }
        }
      }

      def checkTypes(operands: List[Tree]): List[TypeIdentity] = {
        implicit class Regex(sc: StringContext) {
          def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
        }

        // if (operands.length > 0) println(operands(0))
        // if (operands.length > 0) println(operands(0).tpe.toString)
        var typeList: List[TypeIdentity] = List()
        for (operand <- operands) operand.tpe.toString match {
          case r"(?:(?:bitstream\.types\.Matrix\[)?([\w\.]+)$t\]?)" => t match {
            case "bitstream.types.SBitstream" => typeList = typeList :+ SBITSTREAM
            case "bitstream.types.DBitstream" => typeList = typeList :+ DBITSTREAM
            case "bitstream.types.Bit" => typeList = typeList :+ BIT
            case "Int" | "Long" | "Double" => typeList = typeList :+ NUMERIC
            case "bitstream.simulator.units.DelayBuffer" => typeList = typeList :+ BUFFER
            case "bitstream.simulator.units.SDM" => typeList = typeList :+ SDM
            case "Module.this.params.type" => typeList = typeList :+ CONSTANT
            case r"(?:.*\.type)" if inputMap contains operand.toString =>
              typeList = typeList :+ inputMap(operand.toString)
            case "bitstream.types.Matrix.type" => // ignore Matrix objects (eg. Matrix.norm())
            case "bitstream.types.SBitstream.type" => // ignore SBitstream objects (eg. SBitstream.sqrt())
            case default => warning(s"Unrecognized type in checkTypes(): $default")
          }
          case r"(?:(?:Double|Int|Long)\([\d\.\-]+E?[\d\-]*\))" =>
            typeList = typeList :+ NUMERIC
          case default => warning(s"Unrecognized type in checkTypes(): $default")
        }

        // print("[TYPE LIST] ")
        // println(typeList)
        typeList
      }

      def detectLHSOp(tree: Tree): Tree = tree match {
        case Apply(Apply(dontcare, List(src)), implicits) => {
          if (dontcare.toString contains "Ops") src
          else tree
        }
        case _ => tree
      }

      def detectBitConversion(tree: Tree): Tree = tree match {
        case Apply(dontcare, List(src)) => {
          if (dontcare.toString == "types.this.Bit.bit2Int"
              || dontcare.toString == "types.this.Bit.int2Bit") {
            src
          } else tree
        }
        case _ => tree
      }

      def detectInternalUnit(tree: Tree): Tree = {
        var outputTree: BitSADPlugin.this.global.Tree = EmptyTree
        tree match {
          case Select(dontcare, src) => {
            if (dontcare.toString == "Module.this") {
              outputTree = Ident(src)
              outputTree.setType(tree.tpe)
            } else if (dontcare.toString == "Module.this.params") {
              outputTree = Ident(src)
              outputTree.setType(dontcare.tpe)
            } else outputTree = tree
          }
          case _ => outputTree = tree
        }

        outputTree
      }

      def isMatrixObject(node: Tree): Boolean = node.toString.trim == "bitstream.types.Matrix"

      def isSBitstreamObject(node: Tree): Boolean = node.toString.trim == "bitstream.types.SBitstream"

      def getSize(name: String, typeId: TypeIdentity): (String, Int, Int) = {
        if (netList contains name) {
          val size = netList.getSize(name)
          (name, size._1, size._2)
        } else if (checkConstant(name)) {
          val fxp = convertConstantToFxp(name)
          bitWidthList = bitWidthList :+ (fxp._2, fxp._3)
          (fxp._1, fxp._2, 1)
        } else if (typeId == CONSTANT) {
          (name, -1, -1)
        } else {
          // node does not exist yet, ask for user definition
          val line =
            StdIn.readLine(s"What is the size of $name? (format: <rows><space><cols>)\n")
          val Array(a, b) = line.split("\\s+").map(_.toInt)

          // if its in output list, need to do extra work
          if ((outList contains name) && feedbackMode) {
            // feedback node
            // "*_init" is from outside (initial value sent into module)
            // "*_decor" is coming from decorrelator node output
            // "*_in" is the value chosen between init and decor
            addFeedback(name, s"${name}_init", s"${name}_decor", s"${name}_in", (a, b), netList)
              match {case(list, str) => {netList = list; funcBody = funcBody + str}}

            netList.addInputs(List(s"${name}_init"), List((a, b)), List(true))
            (s"${name}_in", a, b)
          } else {
            if (typeId == SBITSTREAM)
              netList.addInputs(List(name), List((a, b)), List(true))
            else
              netList.addInputs(List(name), List((a, b)), List(false))
            (name, a, b)
          }
        }
      }

      def convertConstantToFxp(constant: String): (String, Int, Int) = {
        val constDouble = math.abs(constant.toDouble)

        // Get integer width
        var x = math.floor(constDouble)
        val intWidth = if (x > 0) math.ceil(math.log(x) / math.log(2)).toInt else 0

        // Get frac width
        x = constDouble - x
        val fracWidth = if (x > 0) math.ceil(-1 * math.log(x) / math.log(2)).toInt else 0

        val bitWidth = intWidth + fracWidth + 1

        // Get integer representation
        var intRepresentation = math.floor(constDouble * math.pow(2, fracWidth)).toInt

        val sign = if (math.signum(constant.toDouble).toInt > 0) "" else "-"
        (s"${sign}BITWIDTH_REPLACE'b${getBinString(intRepresentation, bitWidth)}", bitWidth, intWidth)
      }

      def getBinString(x: Int, bitWidth: Int): String = {
        if (x > 0) getBinStringHelper(x)
        else {
          var xStr = getBinStringHelper(math.abs(x))
          val padLen = bitWidth - xStr.length()
          xStr = ("0" * padLen) + xStr
          val body = (c: Char, t: Tuple2[String, Boolean]) => c match {
            case '0' => {
              if (t._2) ("1" + t._1, true)
              else ("0" + t._1, false)
            }
            case '1' => {
              if (t._2) ("0" + t._1, true)
              else ("1" + t._1, true)
            }
          }
          val out = xStr.foldRight(("", false))(body)
          out._1
        }
      }

      def getBinStringHelper(x: Int): String = x match {
        case 0 | 1 => s"${x}"
        case _ => s"${getBinStringHelper(x / 2)}${x % 2}"
      }

      def replaceBitWidth(str: String): String = {
        if (!bitWidthList.isEmpty) {
          val intWidth = bitWidthList.unzip._2.max
          val bitWidth = if (intWidth < 2) bitWidthList.unzip._1.max + (2 - intWidth)
                         else bitWidthList.unzip._1.max
          str.replaceAll("BITWIDTH_REPLACE", bitWidth.toString)
        } else str
      }

      // def getOutList(tree: List[Tree]): List[String] = {
      //   var outputs: List[String] = List()
      //   for (node @ ValDef(mods, opd, tpt, rhs) <- tree if opd.toString.trim == "outputList") {
      //     rhs match {
      //       case Apply(_, list) => outputs = for (output <- list) yield {
      //         println(output)
      //         val q"new (String, Int, Int)(${name: String}, ${r: Int}, ${c: Int})" = output
      //         name
      //       }
      //     }
      //   }

      //   outputs = outputs.map((str: String) => str.drop(1).dropRight(1))
      //   outputs
      // }

      def getDelays(buffNames: List[String], tree: List[Tree]): List[String] = {
        var delayList: List[String] = List()
        for (node @ ValDef(mods, opd, tpt, rhs) <- tree
             if (rhs.toString contains "DelayBuffer") && (buffNames contains opd.toString.trim)) {
          val delay = detectInternalUnit(rhs.children(1))
          val typeId = checkTypes(List(delay)).head
          if (typeId == CONSTANT) delayList = delayList :+ delay.toString
          else delayList = delayList :+ reduceConstantExpr(delay).toInt.toString
        }

        delayList
      }

      def getFeedbackMode(tree: List[Tree]): Boolean = {
        var feedbackMode : Boolean = true
        for (node @ ValDef(mods, opd, tpt, rhs) <- tree if opd.toString.trim == "feedbackMode" ) {
          feedbackMode = (rhs.toString.toInt == 1)
        }
        feedbackMode
      }

      def getInputs(inputs: List[ValDef]): Map[String, TypeIdentity] = {
        val nameTypePairs = for (input @ ValDef(_, name, tpt, _) <- inputs) yield {
          input.setType(tpt.tpe)
          (name.toString, checkTypes(List(input)).head)
        }
        nameTypePairs.toMap
      }

      def printVerilog(funcBody: String) : Unit = {
        val name = StdIn.readLine(s"What is the module name? (no spaces)\n")

        // open a file to be written
        val file = new File(s"./$name.v")
        println(s"Output Verilog written to: ${file.getCanonicalPath}")
        val bw = new BufferedWriter(new FileWriter(file))

        // set user-defined params
        val mapper = (v: Double) => {
          val fxp = convertConstantToFxp(v.toString)
          bitWidthList = bitWidthList :+ (fxp._2, fxp._3)
          fxp._1
        }
        var params = imports.params.map(x => (x._1, mapper(x._2)))

        // write Verilog to file
        var bitWidthStr = ""
        var intWidthStr = ""
        if (!bitWidthList.isEmpty) {
          var bitWidth = bitWidthList.unzip._1.max
          var intWidth = bitWidthList.unzip._2.max

          if (intWidth < 2) {
            bitWidth = bitWidth + (2 - intWidth)
            intWidth = 2
          }

          bitWidthStr = bitWidth.toString
          intWidthStr = intWidth.toString
        }
        bw.write(printTopLevel(name, netList, bitWidthStr, intWidthStr))
        bw.write(replaceBitWidth(printUserParams(params)))
        if (feedbackMode) bw.write(printInputSwitch())
        bw.write(replaceBitWidth(funcBody) + "\n")
        bw.write("endmodule")

        // close the output file
        bw.close()
      }

    } // end of class BitSADPluginPhase

  } // end of object Component extends PluginComponent

} // end of class BitSADPlugin