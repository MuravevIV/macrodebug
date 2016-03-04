package macrodebug

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros._

object NumericOpsMacro {


    implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): NumericOps[T] = new NumericOps[T](x)

    //noinspection AccessorLikeMethodIsEmptyParen
    class NumericOps[T](lhs: T)(implicit T: Numeric[T]) {

        def +(rhs: T): Any = macro NumericOps.+[T]

        def -(rhs: T): Any = macro NumericOps.-[T]

        def *(rhs: T): Any = macro NumericOps.*[T]

        def unary_-(): Any = macro NumericOps.unary_-[T]

        def abs(): Any = macro NumericOps.abs[T]

        def signum(): Any = macro NumericOps.signum[T]

        def toInt(): Any = macro NumericOps.toInt[T]

        def toLong(): Any = macro NumericOps.toLong[T]

        def toDouble(): Any = macro NumericOps.toDouble[T]
    }

    object NumericOps {
        def +[T](c: whitebox.Context)(rhs: c.Expr[T]) = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.plus(lhs.splice, rhs.splice))
        }

        def -[T](c: whitebox.Context)(rhs: c.Expr[T]) = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.minus(lhs.splice, rhs.splice))
        }

        def *[T](c: whitebox.Context)(rhs: c.Expr[T]) = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.times(lhs.splice, rhs.splice))
        }

        def unary_-[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.negate(lhs.splice))
        }

        def abs[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.abs(lhs.splice))
        }

        def signum[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.signum(lhs.splice))
        }

        def toInt[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.toInt(lhs.splice))
        }

        def toLong[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.toLong(lhs.splice))
        }

        def toDouble[T](c: whitebox.Context)() = {
            val (numeric, lhs) = extractNumericAndLhs[T](c)
            c.universe.reify(numeric.splice.toDouble(lhs.splice))
        }

        def extractNumericAndLhs[T](c: whitebox.Context): (c.Expr[Numeric[T]], c.Expr[T]) = {
            import c.universe._

            c.prefix.tree match {
                case Apply(Apply(TypeApply(_ /*infixNumericOps*/ , _), List(lhs)), List(numeric)) =>
                    (c.Expr(numeric), c.Expr(lhs))
                case t =>
                    c.abort(c.enclosingPosition, "unexpected tree: " + show(t))
            }
        }
    }
}
