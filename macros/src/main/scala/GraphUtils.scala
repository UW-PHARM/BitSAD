package bitstream.macros.internal

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._ // shortcuts
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.Context

case class OpNode(val op: String, private var _inputs: List[String], private var _output: String) {

  def inputs: List[String] = _inputs
  def inputs_=(inputs: List[String]) = {
    _inputs = inputs
  }

  def output: String = _output
  def output_=(output: String) = {
    _output = output
  }

  def ==(that: OpNode): Boolean =
    (this.op == that.op) && (this.output == that.output) && this.inputs.zip(that.inputs).forall((x: Tuple2[String, String]) => x._1 == x._2)

}

object GraphUtils {

  def printGraph(g: Graph[OpNode, DiEdge], start: OpNode, level: Int): Unit = {
    var root = g get start
    var padding = "  " * level
    println(s"${padding}op: ${start.op}")
    println(s"${padding}output: ${start.output}")
    println(s"${padding}srcs: ${start.inputs}")
    for (dest <- root.outNeighbors.toList.map(_.toOuter)) {
      printGraph(g, dest, level + 1)
    }
  }

  def addOp(g: Graph[OpNode, DiEdge], op: OpNode): Graph[OpNode, DiEdge] = {
      var newGraph = g + op

      for (input <- op.inputs) {
        var inNodes = (newGraph.nodes.toOuter filter ((x: OpNode) => x.output == input)).toList
        if (inNodes.nonEmpty)
          newGraph.add((inNodes(0) ~> op))
      }

      var outNodes = newGraph.nodes.toOuter filter ((x: OpNode) => x.inputs contains op.output)
      for (dest <- outNodes.toList) {
        newGraph.add((op ~> dest))
      }

      newGraph
    }

}