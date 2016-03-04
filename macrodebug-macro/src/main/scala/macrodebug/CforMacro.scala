package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object CforMacro {

    import MacroUtil._

    /**
      * This call:
      * {{{
      * cfor(zero = 0)(okay = _ < 10, next = _ += 2) { println(_) }
      * }}}
      *
      * Translates to:
      * {{{
      * val a = zero
      * while (okay(a)) {
      *   act(a)
      *   a = next(a)
      * }
      * }}}
      * where the bodies of `okay`, `next`, and `act` are inlined.
      */
    // Suggested by Rex Kerr here: http://www.scala-lang.org/node/9809
    def cfor[A](zero: A)(okay: A => Boolean, next: A => A)(act: A => Unit): Unit =
    macro cforImpl[A]

    def cforImpl[A: c.WeakTypeTag]
    (c: whitebox.Context)
    (zero: c.Expr[A])
    (okay: c.Expr[A => Boolean], next: c.Expr[A => A])
    (act: c.Expr[A => Unit]): c.Expr[Unit] = {

        val t = c.universe.reify {
            var elem: A = zero.splice
            while (okay.splice(elem)) {
                act.splice(elem)
                elem = next.splice(elem)
            }
        }
        c.inlineAndReset(t)
    }
}
