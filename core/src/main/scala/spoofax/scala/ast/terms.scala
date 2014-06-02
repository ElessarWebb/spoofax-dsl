package spoofax.scala

import shapeless._

package object ast {
	// an AST is of course nothing else then just the root of the AST
	// which is a Term.
	type AST = Node
	type SourcePosition = (Int, Int)

	object TheProgram extends Node() with Leaf

	/**
	 * class for keys of properties of term
 	 */
	sealed class TermProperty
	val SourcePosition = new TermProperty()

	/**
	 * Class that describes what annotations are allowed on terms
	 * @tparam K type of the key
	 * @tparam V type of value
	 */
	class TermAnnotation[K, V]
	implicit val SourcePositionAnnotation = new TermAnnotation[TermProperty, SourcePosition]()
}

package ast {

	abstract class Node() {
		def children: Seq[Node]

		/**
		 * Fold operation on an AST.
		 * First processes this node, than it's children from left to right, recursively
		 *
		 * @param z initial value
		 * @param on_enter action to perform on entering of the term
		 * @param on_leave action to perform on leaving the term; defaults to identity
		 */
		def topdown[S](z: S)(on_enter: (S, Node) => S, on_leave: (S, Node) => S = (x: S, y: Node) => x): S = {
			on_leave(
				this.children.foldLeft(on_enter(z, this)) {
					case (acc, child) => child.topdown(acc)(on_enter, on_leave)
				},
				this
			)
		}

		/**
		 * Fold operation on an AST.
		 * First processes it's children from left to right, recursively, then itself
		 *
		 * @param z initial value
		 * @param f fold function
		 */
		def bottomup[S](z: S)(f: (S, Node) => S): S = {
			f(
				this.children.foldLeft(z) {
					(r, t) => t.bottomup(r)(f)
				},
				this
			)
		}
	}

	trait Leaf extends Node {
		override def children = Nil
	}

	/*
	import shapeless._
	import ops.hlist
	import shapeless.PolyDefns._

	/**
	 *
	 * topdown is a polymorphic function that applies a given function `s`
	 * everywhere where possible, recursively
	 *
	 * @param f
	 * @tparam T
	 */
	class topdown[T](f: T => T) extends Poly1 {
		object childrenDefault extends Poly1 {
			implicit def caseDefault[T] = at[T](_ => HNil)
		}
		object children extends childrenDefault {
			implicit def caseTuple[U, V] = at[(U,V)]{ case (x,y) => x :: y :: HNil }
		}

		implicit def caseT(implicit mapper: Mapper[T]) = at[T] { x =>
			val res = f(x)
			// map topdown(f) over the children of the result
			// this will recurse down

		}

		implicit def default[Other] = at[Other] { x =>
		}
	}*/
}
