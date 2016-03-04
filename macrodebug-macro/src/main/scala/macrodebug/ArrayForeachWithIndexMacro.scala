package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object ArrayForeachWithIndexMacro {

    import MacroUtil._

    /**
      * Fast, indexed, foreach over an array, translated to a while loop.
      *
      * {{{
      *  arrayForeachWithIndex(as)((a, i) => println((a, i)))
      * }}}
      *
      * Translated to:
      * {{{
      * {
      *  val $array = as
      *  var $i = 0
      *  val $len = array.length
      *  while ($i < $len) {
      *    val $a = array.apply($i)
      *    f($a, $i);
      *    $i += 1
      *  }
      * }
      * }}}
      * where the `f` is inlined.
      */
    def arrayForeachWithIndex[A](array: Array[A])(f: (A, Int) => Unit): Unit =
    macro arrayForeachWithIndexImpl[A]

    def arrayForeachWithIndexImpl[A: c.WeakTypeTag]
    (c: whitebox.Context)
    (array: c.Expr[Array[A]])
    (f: c.Expr[(A, Int) => Unit]): c.Expr[Unit] = {

        val expr = c.universe.reify {
            val a = array.splice
            var i = 0
            val len = a.length
            while (i < len) {
                val elem = a(i)
                f.splice(elem, i)
                i += 1
            }
        }

        c.inlineAndReset(expr)
    }
}
