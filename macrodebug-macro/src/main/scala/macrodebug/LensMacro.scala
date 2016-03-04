package macrodebug

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros._

object LensMacro {

    /**
      * Automatic lens generation.
      *
      * {{{
      * case class Foo(a: Int)
      * val l = lens[Foo].a
      * val foo = Foo(0)
      * l._1(foo) // 0
      * l._2(foo, 1) // Foo(1)
      * }}}
      */
    def lens[T] = new Lenser[T]

    class Lenser[T] extends Dynamic {
        def selectDynamic(propName: String) = macro Lenser.selectDynamic[T]

        def applyDynamic(propName: String)() = macro Lenser.applyDynamic[T]
    }

    object Lenser {
        def selectDynamic[T: c.WeakTypeTag](c: whitebox.Context)(propName: c.Expr[String]) =
            applyDynamic[T](c)(propName)()

        def applyDynamic[T: c.WeakTypeTag]
        (c: whitebox.Context)
        (propName: c.Expr[String])
        ()
        = {
            import c.universe._
            // Why doesn't this work if I try to use scala.Tuple2's symbol?
            def Tuple2Module = Select(Ident(TermName("scala")), TermName("Tuple2"))
            def mkParam(name: String, tpe: Type) =
                ValDef(Modifiers(Flag.PARAM), TermName(name), TypeTree(tpe), EmptyTree)

            import internal.gen._
            //println(showRaw(_this))
            val t = (c.prefix.tree, propName.tree) match {
                case (TypeApply(
                Select(
                _, lensMethodTermName
                ), List(tpe)), Literal(Constant(methodName: String))) =>
                    val getterMember = tpe.tpe.member(TermName(methodName)) orElse {
                        c.abort(c.enclosingPosition, "value " + methodName + " is not a member of " + tpe.tpe)
                    }
                    val memberType = getterMember.typeSignatureIn(tpe.tpe) match {
                        case NullaryMethodType(memberTypeValue) => memberTypeValue
                        case _ => c.abort(c.enclosingPosition, "member %s is not a field".format(methodName))
                    }
                    val getter = Function(
                        List(mkParam("a$", tpe.tpe)),
                        Select(Ident(TermName("a$")), TermName(methodName))
                    )
                    val setter = Function(
                        List(mkParam("a$", tpe.tpe), mkParam("x$", memberType)),
                        Apply(
                            Select(Ident(TermName("a$")), TermName("copy")),
                            List(AssignOrNamedArg(Ident(TermName(methodName)), Ident(TermName("x$"))))
                        )
                    )
                    mkMethodCall(Select(Tuple2Module, TermName("apply")), List(getter, setter))
                case x =>
                    c.abort(c.enclosingPosition, "unexpected c.prefix tree: " + x)
            }
            c.Expr[Any](c.untypecheck(t))
        }
    }
}
