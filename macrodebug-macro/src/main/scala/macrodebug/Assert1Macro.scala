package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object Assert1Macro {

    /**
      * Assert that `cond` is true. The tree of `cond` is used as the assertion message.
      */
    def assert1(cond: Boolean): Unit = macro assert1Impl

    def assert1Impl(c: whitebox.Context)(cond: c.Expr[Boolean]) = {
        import c.universe._
        val condCode = c.Expr[String](Literal(Constant(show(cond.tree))))
        c.universe.reify {
            assert(cond.splice, condCode.splice)
            ()
        }
    }
}
