package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object IteratorForeachMacro {

    import MacroUtil._

    /**
      * Converts:
      * {{{
      *  iteratorForeach(iterator)(a => <body>)
      * }}}
      *
      * To:
      * {{{
      * while(iterator.hasNext) f(iterator.next())
      * }}}
      * where `f` is inlined.
      */
    def iteratorForeach[A](iterator: Iterator[A])
                          (act: A => Unit): Unit =
    macro iteratorForeachImpl[A]

    def iteratorForeachImpl[A: c.WeakTypeTag]
    (c: Context)
    (iterator: c.Expr[Iterator[A]])
    (act: c.Expr[A => Unit]): c.Expr[Unit] = {

        val e = c.universe.reify {
            val i = iterator.splice
            while (i.hasNext) {
                val elem = i.next()
                act.splice(elem)
            }
        }
        c.inlineAndReset(e)
    }
}
