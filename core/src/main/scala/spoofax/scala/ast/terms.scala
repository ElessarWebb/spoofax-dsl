package spoofax.scala {

package object ast {
	// an AST is of course nothing else then just the root of the AST
	// which is a Term.
	type AST = Term
	type SourcePosition = (Int, Int)
}

package ast {
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