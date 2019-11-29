package bitstream.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {

  // def getVarName(x: Any): String = macro impl

  // def impl(c: Context)(x: c.Tree): c.Tree = {
  //   import c.universe._
  //   val p = x match {
  //     case Select(_, TermName(s)) => s
  //     case _ => ""
  //   }
  //   q"$p"
  // }

  def simulatable(expr: Any): Any = macro simulatable_impl

  def simulatable_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    val annotatedTree = expr.tree match {
      case Block(statements, ret) => {
        println("Hello from macro")
        expr.tree
      }
      case _ => expr.tree
    }

    c.Expr[Any](annotatedTree)
  }

  private def addSimulation(c: Context)(statement: c.Tree): c.Tree = {
    ???
  }

}