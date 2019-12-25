package bitstream.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {

  def simulatable[T](expr: T): T = macro simulatable_impl[T]

  def simulatable_impl[T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    def checkDefaultId(src: Tree) = src.toString match {
      case "types.this.SimulationId.DefaultId" => true
      case _ => false
    }

    def getDefaultId(srcs: List[Tree]) = srcs.indexWhere(checkDefaultId _)

    def addSimulationId(statement: Tree): Tree = statement match {
      case Ident(TermName(src)) => {
        println(s"TermName:   $src")
        statement
      }
      case Apply(Select(src1, TermName(op)), srcs) => {
        // insert id
        val idLoc = getDefaultId(srcs)
        println(s"  Location: $idLoc")
        var newSrcs = srcs
        if (idLoc >= 0) {
          newSrcs = List(q"SimulationId(${src1.toString}, ${""})")
          println(s"  new: ($src1 $op)($newSrcs)")
        } else {
          println(s"""Encountered statement: $statement
                     |  which appears as a binary operator, but RHS contains a list of operands.
                     |  Unsure how to create a SimulationId for this type of statement.""".stripMargin)
        }

        // recursively apply to sources
        var newSrc1 = addSimulationId(src1)

        // treeCopy.Apply(statement, Select(newSrc1, TermName(op)), newSrcs)
        atPos(statement.pos.focus)(q"$src1.${TermName(op)}(..$newSrcs)")
      }
      case Apply(Apply(Select(src1, TermName(op)), List(src2)), srcs) => {
        println(s"3-Function: ($src1 $op $src2)($srcs)")

        // insert id
        val idLoc = getDefaultId(srcs)
        println(s"  Location: $idLoc")
        var newSrcs = srcs
        if (idLoc >= 0) {
          newSrcs = (srcs.slice(0, idLoc) :+
                    q"SimulationId(${src1.toString}, ${src2.toString})") ++
                    srcs.slice(idLoc + 1, srcs.length)
          println(s"  new: ($src1 $op $src2)($newSrcs)")
        } else {
          newSrcs = srcs :+
                    q"SimulationId(${src1.toString}, ${src2.toString})"
          println(s"  new: ($src1 $op $src2)($newSrcs)")
        }

        // recursively apply to sources
        var newSrc1 = addSimulationId(src1)
        var newSrc2 = addSimulationId(src2)

        // treeCopy.Apply(statement, Apply(Select(newSrc1, TermName(op)), List(newSrc2)), newSrcs)
        atPos(statement.pos.focus)(q"($src1 ${TermName(op)} $src2)(..$newSrcs)")
      }
      case _ => {
        println(showRaw(statement))
        statement
      }
    }

    val annotatedTree = expr.tree match {
      case Block(statements, ret) => {
        var newStatements =
          for (statement <- statements) yield statement match {
            case Assign(opd, rhs) => treeCopy.Assign(statement, opd, addSimulationId(rhs))
            case ValDef(mods, opd, tpt, rhs) => treeCopy.ValDef(statement, mods, opd, tpt, addSimulationId(rhs))
            case _ => statement
          }

        treeCopy.Block(expr.tree, newStatements, ret)
      }
      case Apply(Select(src1, TermName(op)), srcs) => addSimulationId(expr.tree)
      case Apply(Apply(Select(src1, TermName(op)), List(src2)), srcs) => addSimulationId(expr.tree)
      case _ => expr.tree
    }

    println()
    println(annotatedTree)
    c.Expr[T](annotatedTree)
  }

}