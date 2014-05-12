package spoofax.scala.macros

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

object Term {
	def apply(): Int = macro apply_impl

	def apply_impl(c: Context)(): c.Expr[Int] = {
		import c.universe._
		/*val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
			case (param: ValDef) :: (rest@(_ :: _)) => (param, rest)
			case (param: TypeDef) :: (rest@(_ :: _)) => (param, rest)
			case _ => (EmptyTree, inputs)
		}
		println((annottee, expandees))
		val outputs = expandees
		val outputs = q"class Blam(x: Int)"
		c.Expr(Block(outputs, Literal(Constant(()))))*/
		reify {
			42
		}
	}
}
