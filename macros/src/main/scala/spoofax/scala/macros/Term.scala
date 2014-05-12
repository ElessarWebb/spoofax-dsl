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
	def valdefs_to_params(c: Context)(params: Seq[c.universe.ValDef]): c.Expr[Any] = {
		import c.universe._

		params.map {
				case q"$mods val $name: $tpt = $default" =>
					q""
				case _ => c.abort(c.enclosingPosition, "Expected val definition")
		}

		c.Expr(params(0))
	}

	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		val inputs = annottees.map(_.tree).toList
		val (cls, comp) = annottees.map(_.tree) match {
			// todo: check modifiers/parents et al
			case cd@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$_ } with ..$_ { $self => ..$stats }" :: tail =>
				//valdefs_to_params(c)(paramss)
				(
					q"""import spoofax.scala.ast.Term
				 			case class $tpname(...$paramss) extends Term(Nil) { $self =>
								..$stats
							}
					 """,
					q"""object ${TermName(tpname.decodedName.toString)} {
				 				def companion(...$paramss) = {
									42
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

