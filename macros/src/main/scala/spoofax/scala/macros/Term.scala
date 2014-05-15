package spoofax.scala.macros

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

/**
 * Macro annotation that allows you to write
 *
 * @@term class A(c: ChildL, c: ChildR)
 *
 * such that it is expanded to:
 *
 * case class A(l: ChildL, r: ChildR, annotations: TermAnnotations) extends Term(List(l, r), annotations)
 * object A {
 * 	def apply(l: ChildL, r: ChildR) = A(l, r, NoAnnotations())
 * 	def unapply(c: A): Option[(ChildL, ChildR)] = {
 * 		if(c == null) return None
 * 	 	else return Some(c.l, c.r)
 * 	}
 * }
 **/
class term extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro termMacro.impl
}

object termMacro {

	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		val inputs = annottees.map(_.tree).toList

		val (cls, comp) = annottees.map(_.tree) match {

			case cd@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$_ } with ..$_ { $self => ..$stats }" :: tail =>

				val paramnames = paramss.head.map {
					case q"$mods val $name: $tpt = $default" => name
				}

				val ctorparams = List(paramss.head ++ Seq(
					q"val position: Option[SourcePosition] = None"
				))

				val ctorname = TermName(tpname.decodedName.toString)
				(
					q"""class $tpname(...$ctorparams) extends Term { $self =>
								def children() = {
									List(..$paramnames)
								}

								..$stats
							}
					 """,
					q""" object $ctorname {
				 				def apply(...$paramss): $tpname = new $tpname(..$paramnames, None)

				 				def unapply(t: $tpname): Option[(Int, Int)] = {
									Some(Tuple2(3, 4))
								}
				  		}
					"""
				)

			case head :: tail =>
				c.abort(c.enclosingPosition, s"The @Term annotation is for case classes, found $head")
		}

		c.Expr[Any](Block(List(cls, comp), Literal(Constant(()))))
	}
}

