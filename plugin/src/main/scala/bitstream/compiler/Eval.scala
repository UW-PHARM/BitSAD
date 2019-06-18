package bitstream.compiler
package eval

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

// import bitstream.types._
// import bitstream.simulator.units._

// Based on code from:
// https://gist.github.com/xuwei-k/9ba39fe22f120cb098f4
object Eval {

  def apply[A](treeStr: String): A = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(treeStr)
    toolbox.eval(tree).asInstanceOf[A]
  }

}

// object Typecheck {

//   def apply[A](tree: A): A = {
//     val toolbox = currentMirror.mkToolBox()
//     // // val tree = toolbox.parse(replaceRelativePackages(treeStr))
//     toolbox.typecheck(tree.asInstanceOf[reflect.runtime.universe.Tree]).asInstanceOf[A]
//   }

//   private def replaceRelativePackages(treeStr: String): String =
//     treeStr.replaceAll("types.this", "_root_.bitstream.types")

// }