package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object DesugarMacro {

    /**
      * @return the tree of `a` after the typer, printed as source code.
      */
    def desugar(a: Any): String = macro desugarImpl

    def desugarImpl(c: whitebox.Context)(a: c.Expr[Any]) = {
        import c.universe._

        val s = show(a.tree)
        c.Expr(
            Literal(Constant(s))
        )
    }
}
