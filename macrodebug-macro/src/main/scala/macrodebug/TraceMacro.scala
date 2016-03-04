package macrodebug

import scala.language.experimental.macros
import scala.reflect.macros._

object TraceMacro {

    /**
      * Trace execution on `c`, by printing the values of sub-expressions
      * to standard out.
      */
    def trace[A](expr: A) = macro traceImpl[A]

    def traceImpl[A: c.WeakTypeTag](c: whitebox.Context)(expr: c.Expr[A]): c.Expr[A] = {
        import c.universe._

        object tracingTransformer extends Transformer {
            def insertTrace(t: Tree): Tree = {
                val expr = c.Expr[Any](t)
                val exprCode = c.Expr[String](Literal(Constant(show(t))))
                val exprTpe = c.Expr[String](Literal(Constant(show(t.tpe))))

                (c.universe.reify {
                    val result = expr.splice
                    println("%s = %s: %s".format(exprCode.splice, result, exprTpe.splice))
                    result
                }).tree
            }

            override def transform(tree: Tree): Tree = {
                tree match {
                    case Apply(_, _) =>
                        val result = super.transform(tree)
                        insertTrace(tree)
                    case Select(_, _: TermName) =>
                        val result = super.transform(tree)
                        // qual.meth(...)
                        // \-------/
                        //    don't trace this part.
                        result.tpe match {
                            case MethodType(_, _) | PolyType(_, _) => result
                            case _ => insertTrace(tree)
                        }
                    case _ =>
                        super.transform(tree)
                }
            }
        }
        val t = tracingTransformer.transform(expr.tree)
        c.Expr[A](c.untypecheck(t))
    }
}
