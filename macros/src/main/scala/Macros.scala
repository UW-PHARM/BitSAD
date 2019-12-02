package bitstream.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import bitstream.macros.internal._
import bitstream.macros.decorrinsertion._

object Macros {

  def decorrelate(expr: Any): Any = macro decorrelate_impl

  def decorrelate_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    var dfg = Graph[OpNode, LDiEdge]()

    def addStatement(g: Graph[OpNode, LDiEdge], statement: Tree, output: String):
        Graph[OpNode, LDiEdge] = {
      if (statement.children.length == 0) g
      else statement match {
        case Apply(Select(src, op), vsrc) if op.toString.trim == "loop" => {
          val body = (g: Graph[OpNode, LDiEdge], x: Tree) => addStatement(g, x, x.toString)
          var newGraph = vsrc.foldLeft(g)(body)

          var inputs = vsrc.map(_.toString)
          var node = OpNode(src.toString, output)
          node.inputs = inputs

          GraphUtils.addOp(g, node)
        }
        case Select(src1, op) if op.toString == "toDouble" || op.toString == "toInt" =>
          addStatement(g, src1, output)
        case Select(src1, op) => {
          var newGraph = addStatement(g, src1, src1.toString)

          var inputs = List(src1.toString)
          var node = OpNode(op.toString, output)
          node.inputs = inputs

          GraphUtils.addOp(newGraph, node)
        }
        case Apply(Select(src1, op), List(src2)) => {
          var newGraph = addStatement(g, src1, src1.toString)
          newGraph = addStatement(g, src2, src2.toString)

          var inputs = List(src1.toString, src2.toString)
          var node = OpNode(op.toString, output)
          node.inputs = inputs

          GraphUtils.addOp(newGraph, node)
        }
        case Apply(Apply(Select(src1, op), List(src2)), List(src3)) => {
          var newGraph = addStatement(g, src1, src1.toString)
          newGraph = addStatement(g, src2, src2.toString)

          var inputs = List(src1.toString, src2.toString)
          var node = OpNode(op.toString, output)
          node.inputs = inputs

          GraphUtils.addOp(newGraph, node)
        }
        case _ => throw new Exception(s"Could match statement structure: $statement")
      }
    }

    val annotatedTree = expr.tree match {
      case Block(statements, ret) => {
        for (statement <- statements) statement match {
          case Assign(opd, rhs) => dfg = addStatement(dfg, rhs, opd.toString)
          case ValDef(_, opd, _, rhs) => dfg = addStatement(dfg, rhs, opd.toString)
          case _ => throw new Exception(s"Unrecognized statement: $statement")
        }

        dfg = GraphUtils.addEndPoints(dfg)
        dfg = GraphUtils.updateIndexSets(dfg)
        println("Pre-Decorrelation:")
        GraphUtils.printGraph(dfg)

        dfg = GreedyAlgorithm.execute(dfg)

        println("Post-Decorrelation:")
        GraphUtils.printGraph(dfg)

        println(s"Correlated: ${GraphUtils.isGraphCorrelated(dfg)}")
        expr.tree
      }
      case _ => expr.tree
    }

    c.Expr[Any](annotatedTree)
  }

}