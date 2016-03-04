package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object LogMacro {

    /**
      * ```
      * log("".isEmpty) // prints: "".isEmpty = true
      * ```
      */
    def log[A](a: A): A = macro logImpl[A]

    def logImpl[A: c.WeakTypeTag](c: whitebox.Context)(a: c.Expr[A]): c.Expr[A] = {
        import c.universe._
        val aCode = c.Expr[String](Literal(Constant(show(a.tree))))
        c.universe.reify {
            val result = a.splice
            println(aCode.splice + " = " + result)
            result
        }
    }
}
