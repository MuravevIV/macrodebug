package macrodebug

import scala.language.implicitConversions
import scala.reflect.macros._

object MacroUtil {

    implicit def Util(context: whitebox.Context): Util[context.type] = new Util[context.type](context)

    class Util[C <: whitebox.Context](val c: C) {

        import c.universe._

        def inlineAndReset[T](expr: c.Expr[T]): c.Expr[T] =
            c.Expr[T](c untypecheck inlineApplyRecursive(expr.tree))

        /**
          * Reursively transforms `tree`, inlining direct function
          * application.
          *
          * In:
          * `((p1, p2, ... pN) => <body>).apply(a1, a2, ..., aN)`
          *
          * Out:
          * ```
          * val p1 = a1; val p2 = a2; ... val pN = aN;
          * <body>
          * ```
          */
        def inlineApplyRecursive(tree: Tree): Tree = {
            val ApplyName = TermName("apply")

            object inliner extends Transformer {
                override def transform(tree: Tree): Tree = {
                    tree match {
                        case ap@Apply(Select(prefix, ApplyName), args) =>
                            prefix match {
                                case Function(params, body) =>
                                    if (params.length != args.length)
                                        c.abort(c.enclosingPosition, "incorrect arity: " +(params.length, args.length))
                                    // val a$0 = args(0); val b$0 = args(1); ...
                                    val paramVals = params.zip(args).map {
                                        case (ValDef(_, pName, _, _), a) =>
                                            ValDef(Modifiers(), TermName("" + pName + "$0"), TypeTree(), a)
                                    }
                                    // val a = a$0; val b = b$0
                                    val paramVals2 = params.zip(args).map {
                                        case (ValDef(_, pName, _, _), a) =>
                                            ValDef(Modifiers(), pName, TypeTree(), Ident(TermName("" + pName + "$0")))
                                    }
                                    // The nested blocks avoid name clashes.
                                    Block(paramVals, Block(paramVals2, body))
                                case x => ap
                            }
                        case _ => super.transform(tree)
                    }
                }
            }
            inliner.transform(tree)
        }
    }

}
