package spoofax.scala.ast

trait Term {
	def children: Seq[Term]

	/**
	 * Fold operation on an AST.
	 * First processes this node, than it's children from left to right, recursively
	 *
	 * @param z initial value
	 * @param f fold function
	 */
	def foldDown[T](z: T)(f: (T, Term) => T): T = {
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
	def foldUp[T](z: T)(f: (T, Term) => T): T = {
		f(
			this.children.foldLeft[T](z) {
				(r: T, t) => t.foldDown(r)(f)
			},
			this
		)
	}
}