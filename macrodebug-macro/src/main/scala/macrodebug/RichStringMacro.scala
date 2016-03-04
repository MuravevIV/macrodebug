package macrodebug

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros._

object RichStringMacro {


    implicit def enrichStringContext(sc: StringContext): RichStringContext = new RichStringContext(sc)

    class RichStringContext(sc: StringContext) {
        // This is how a non-macro version would be implemented.
        // def b() = {
        //   val s = sc.parts.mkString
        //   parseBinary(s).getOrElse(sys.error("invalid binary literal: " + s))
        // }

        /** Binary literal integer
          *
          * {{{
          *  scala> b"101010"
          *  res0: Int = 42
          * }}}
          */
        def b(): Int = macro bImpl
    }


    def bImpl(c: whitebox.Context)(): c.Expr[Int] = {
        def parseBinary(s: String): Int = {
            var i = s.length - 1
            var sum = 0
            var mult = 1
            while (i >= 0) {
                s.charAt(i) match {
                    case '1' => sum += mult
                    case '0' =>
                    case x =>
                        c.abort(c.enclosingPosition, "invalid binary literal")
                }
                mult *= 2
                i -= 1
            }
            sum
        }

        import c.universe._

        val i = c.prefix.tree match {
            // e.g: `c.g.r.m.Macrocosm.enrichStringContext(scala.StringContext.apply("1111"))`
            case Apply(_, List(Apply(_, List(Literal(Constant(const: String)))))) =>
                parseBinary(const)
            case x =>
                c.abort(c.enclosingPosition, "unexpected tree: " + show(x))
        }
        c.Expr[Int](Literal(Constant(i)))
    }
}
