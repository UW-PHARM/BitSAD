package bitstream.macros.internal

import scala.collection.immutable._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.Context

// implicit casting for labeled edges
import scalax.collection.edge.LBase.LEdgeImplicits
object LabelTypeImplicit extends LEdgeImplicits[HashSet[Int]]; import LabelTypeImplicit._

case class OpNode(val op: String, private var _inputs: List[String], private var _output: String) {

  // private var _inSets = new List[HashSet[Int]]()
  // private var _outSet = new Set[Int]()

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

  // def inSets: List[Set[Int]] = _inSets
  // def addInSet(set: Set[Int]): Unit = {
  //   _inSets = _inSets :+ set
  //   OpNode._setOutSet(this)
  // }

  // // def isCorrelated: Boolean = _inSets.map((x: HashSet[Int]) =>
  // //                                           (_inSets - x).foldLeft(false)((acc: Boolean, y: HashSet[Int]) => acc || (x & y).nonEmpty))
  // def isCorrelated: Boolean = OpNode._checkCorrelation(this)

  // def outSet: Set[Int] = _outSet

}

// object OpNode {

//   private def _setOutSet(node: OpNode): Unit = node.op match {
//     case "Decorrelator" => {}
//     case _ => node._outSet = node._inSets.foldLeft(new HashSet[Int]())(++)
//   }

//   private def _checkCorrelation(node: OpNode): Boolean = node.op match {
//     case "Decorrelator" => false
//     case _ =>
//       node._inSets.foldLeft(new HashSet[Int]())(++).size == node._inSets.foldLeft(0)((acc: Int, x: HashSet[Int]) => acc + x.size)
//   }

// }

object GraphUtils {

  private var currentIndex = -1

  def newIndex(): Int = {
    currentIndex += 1
    currentIndex
  }

  def printGraph(g: Graph[OpNode, LDiEdge], start: OpNode, level: Int): Unit = {
    var root = g get start
    var padding = "  " * level
    println(s"${padding}op: ${start.op}")
    println(s"${padding}output: ${start.output}")
    println(s"${padding}out set: ${getOutSet(g, start)}")
    println(s"${padding}srcs: ${start.inputs}")
    for (dest <- root.outNeighbors.toList.map(_.toOuter)) {
      printGraph(g, dest, level + 1)
    }
  }

  def isNodeCorrelated(g: Graph[OpNode, LDiEdge], node: OpNode): Boolean = {
    var inSets: List[HashSet[Int]] = (g get node).incoming.toList.map(innerEdge2UserLabel(_))

    val unionSize = inSets.foldLeft(new HashSet[Int]())((acc: HashSet[Int], x: HashSet[Int]) => acc ++ x).size
    val totalSize = inSets.foldLeft(0)((acc: Int, x: HashSet[Int]) => acc + x.size)

    unionSize < totalSize
  }

  def isGraphCorrelated(g: Graph[OpNode, LDiEdge]): Boolean =
    g.nodes.toOuter.toList.foldLeft(false)((acc: Boolean, x: OpNode) => acc || isNodeCorrelated(g, x))

  def getOutSet(g: Graph[OpNode, LDiEdge], node: OpNode): HashSet[Int] = node.op match {
    case "Variable" => {
      var innerNode = g get node
      var outEdges = innerNode.outgoing.toList.map(innerEdge2UserLabel(_))

      if (outEdges.isEmpty)
        HashSet[Int](newIndex())
      else
        outEdges.head
    }
    case _ => {
      var innerNode = g get node
      var inSets: List[HashSet[Int]] = innerNode.incoming.toList.map(innerEdge2UserLabel(_))

      inSets.foldLeft(new HashSet[Int]())((acc: HashSet[Int], x: HashSet[Int]) => acc ++ x)
    }
  }

  def addOp(g: Graph[OpNode, LDiEdge], op: OpNode): Graph[OpNode, LDiEdge] = {
    var newGraph = g + op

    var inNodes = (newGraph.nodes.toOuter filter ((x: OpNode) => op.inputs contains x.output)).toList
    inNodes.foreach((x: OpNode) => newGraph.add((x ~+> op)(getOutSet(newGraph, x))))

    var outSet = getOutSet(newGraph, op)

    var outNodes = (newGraph.nodes.toOuter filter ((x: OpNode) => x.inputs contains op.output)).toList
    outNodes.foreach((x: OpNode) => newGraph.add((op ~+> x)(outSet)))

    newGraph
  }

  def addEndPoints(g: Graph[OpNode, LDiEdge]): Graph[OpNode, LDiEdge] = {
    var newGraph = g
    var starts = (g.nodes filter ((x: Graph[OpNode, LDiEdge]#NodeT) => x.diPredecessors.isEmpty)).toList.map(_.toOuter)

    for (start <- starts) {
      for (input <- start.inputs) {
        newGraph = addOp(newGraph, OpNode("Variable", List[String](), input))
      }
    }

    newGraph
  }

  private def _updateIndexSetByLevel(g: Graph[OpNode, LDiEdge], nodes: Set[OpNode]):
      (Graph[OpNode, LDiEdge], Set[OpNode]) = {
    var newGraph = g
    var parents = Set[OpNode]()

    for (node <- nodes) {
      var outSet = getOutSet(newGraph, node)
      var outNodes = (g get node).diSuccessors.toList.map(_.toOuter)
      for (dest <- outNodes) {
        newGraph.upsert((node ~+> dest)(outSet))
      }
      parents = parents ++ outNodes
    }

    (newGraph, parents)
  }

  def updateIndexSets(g: Graph[OpNode, LDiEdge]): Graph[OpNode, LDiEdge] = {
    var newGraph = g
    var starts: Set[OpNode] = (g.nodes filter ((x: Graph[OpNode, LDiEdge]#NodeT) => x.diPredecessors.isEmpty)).toSet.map(
                                    (x: Graph[OpNode, LDiEdge]#NodeT) => x.toOuter)

    while (starts.nonEmpty) _updateIndexSetByLevel(newGraph, starts) match {
      case (graph, parents) => {
        newGraph = graph
        starts = parents
      }
    }

    newGraph
  }

}