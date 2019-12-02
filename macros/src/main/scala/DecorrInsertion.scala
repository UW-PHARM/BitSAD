package bitstream.macros.decorrinsertion

import scala.collection.immutable._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import bitstream.macros.internal._

// implicit casting for labeled edges
import scalax.collection.edge.LBase.LEdgeImplicits
import LabelTypeImplicit._

object DecorrInsertion {

  def insertDecorrelator(g: Graph[OpNode, LDiEdge], edge: LDiEdge[OpNode]): Graph[OpNode, LDiEdge] = {
    var decorr = OpNode("Decorrelator", s"${edge._1.output}_decorr")
    decorr.inputs = List(edge._1.output)
    var newGraph = g + decorr
    var src = edge._1
    var dst = edge._2
    newGraph.add((src ~+> decorr)(innerEdge2UserLabel(newGraph get edge)))
    newGraph.add((decorr ~+> dst)(GraphUtils.getOutSet(newGraph, decorr)))
    newGraph.remove(edge)
    var replaceIdx = dst.inputs.indexOf(src.output)
    dst.inputs = (dst.inputs.slice(0, replaceIdx) :+ decorr.output) ++
                  dst.inputs.slice(replaceIdx + 1, dst.inputs.size)
    newGraph = GraphUtils.updateIndexSets(newGraph)

    newGraph
  }

}

object GreedyAlgorithm {

  private def _getParents(g: Graph[OpNode, LDiEdge], nodes: Set[OpNode]): Set[OpNode] = {
    var parents = Set[OpNode]()
    nodes.foreach((x: OpNode) => parents = parents ++ (g get x).diSuccessors.toList.map(_.toOuter))

    parents
  }

  private def _findMaxCorrelatedEdge(g: Graph[OpNode, LDiEdge], node: OpNode): LDiEdge[OpNode] = {
    var inEdges = (g get node).incoming.toList
    var inSets = inEdges.map(innerEdge2UserLabel(_))

    var maxEdge = inEdges.head
    var max = inSets.tail.foldLeft(0)((acc: Int, x: HashSet[Int]) => acc + (inSets.head & x).size)
    for (i <- inEdges.indices if i > 0) {
      var remaining = inSets.slice(0, i) ++ inSets.slice(i + 1, inSets.size)
      var intersection = remaining.foldLeft(0)((acc: Int, x: HashSet[Int]) => acc + (inSets(i) & x).size)

      if (intersection > max) {
        max = intersection
        maxEdge = inEdges(i)
      }
    }

    maxEdge.toOuter
  }

  def execute(g: Graph[OpNode, LDiEdge]): Graph[OpNode, LDiEdge] = {
    var newGraph = g
    var nodes = GraphUtils.getLeaves(newGraph).toSet

    while (nodes.nonEmpty) {
      for (node <- nodes) {
        while (GraphUtils.isNodeCorrelated(newGraph, node)) {
          var maxEdge = _findMaxCorrelatedEdge(newGraph, node)
          newGraph = DecorrInsertion.insertDecorrelator(newGraph, maxEdge)
        }
      }

      nodes = _getParents(newGraph, nodes)
    }

    newGraph
  }

}