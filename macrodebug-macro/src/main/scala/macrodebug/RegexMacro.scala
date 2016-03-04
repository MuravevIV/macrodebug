package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object RegexMacro {

    /**
      * Statically checked version of `"some([Rr]egex)".r`.
      * Invalid regular expressions trigger a compile failure.
      * At runtime, the regex is parsed again.
      *
      * {{{
      *  scala> regex(".*")
      * res0: scala.util.matching.Regex = .*
      *
      * scala> regex("{")
      * <console>:11: error: exception during macro expansion: Illegal repetition
      * {
      *           regex("{")
      *                ^
      * }}}
      */
    def regex(s: String): scala.util.matching.Regex = macro regexImpl

    def regexImpl(c: whitebox.Context)(s: c.Expr[String]): c.Expr[scala.util.matching.Regex] = {
        import c.universe._

        s.tree match {
            case Literal(Constant(string: String)) =>
                string.r // just to check
                c.universe.reify(s.splice.r)
        }
    }
}
