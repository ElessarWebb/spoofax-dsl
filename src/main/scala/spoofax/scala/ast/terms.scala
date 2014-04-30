package spoofax.scala {

package object ast {
	// an AST is of course nothing else then just the root of the AST
	// which is a Term.
	type AST[+T] = Term[T]
	type SourcePosition = (Int, Int)
}

package ast {

/**
 * Terms are parametrized over identifier type T to be able to add more information to identifiers
 * in different compiler stages.
 * e.g. the parser produces a tree of type Term[String], whereas the renamer produces Term[Term]
 */
abstract class Term[+T](pos: SourcePosition) {
	def children: Seq[Term[T]]

	/**
	 * Fold operation on an AST.
	 * First processes this node, than it's children from left to right, recursively
	 *
	 * @param z initial value
	 * @param f fold function
	 */
	def foldDown[S](z: S)(f: (S, Term[T]) => S): S = {
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
	def foldUp[S](z: S)(f: (S, Term[T]) => S): S = {
		f(
			this.children.foldLeft(z) {
				(r, t) => t.foldDown(r)(f)
			},
			this
		)
	}
}