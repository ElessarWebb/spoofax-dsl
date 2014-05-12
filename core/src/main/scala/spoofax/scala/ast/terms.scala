package spoofax.scala {

package object ast {
	// an AST is of course nothing else then just the root of the AST
	// which is a Term.
	type AST = Term
	type SourcePosition = (Int, Int)
	
}

package ast {
	import scala.reflect.macros.whitebox.Context
	import scala.language.experimental.macros
	import scala.annotation.StaticAnnotation

	/**
	 * Macro annotation that allows you to write
	 *
	 * @TerM case class A(c: ChildL, c: ChildR)
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
	object TerM {

		def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
			import c.universe._

			val inputs = annottees.map(_.tree).toList
			val outputs = inputs match {
				case (cd@ClassDef(cmods, cname, _, _)) :: tail =>
					val mod = tail match {
						case md@ModuleDef(_, mname, impl)
							if cname.decodedName.toString == mname.decodedName.toString => md
						case _ =>
							// TODO flags copying?
							val ctor = cd.impl.collect {
								case ctor@DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => ctor
							}

							q"""
								$cmods object ${TermName(cname.decodedName.toString)} {
								}
							"""
					}

					List(mod)
			}

			println(outputs)

			c.Expr[Any](Block(inputs ++ outputs, Literal(Constant())))
		}
	}

	class TerM extends StaticAnnotation {
		def macroTransform(annottees: Any*): Any = macro ???
	}

	/**
	 * Term without children
	 */
	trait Leaf extends Term {
		def children = Nil
	}

	abstract class Term(children: Seq[Term]) {

		/**
		 * Fold operation on an AST.
		 * First processes this node, than it's children from left to right, recursively
		 *
		 * @param z initial value
		 * @param f fold function
		 */
		def foldDown[S](z: S)(f: (S, Term) => S): S = {
			this.children.foldLeft(f(z, this)) {
				case (acc, child) => child.foldDown(acc)(f)
			}
		}

		/**
		 * Fold operation on an AST.
		 * First processes it's children from left to right, recursively, then itself
		 *
		 * @param z initial value
		 * @param f fold function
		 */
		def foldUp[S](z: S)(f: (S, Term) => S): S = {
			f(
				this.children.foldLeft(z) {
					(r, t) => t.foldUp(r)(f)
				},
				this
			)
		}
	}

}
}