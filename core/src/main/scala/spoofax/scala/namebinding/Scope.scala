package spoofax.scala.namebinding

import spoofax.scala.ast._
import java.util.concurrent.atomic.AtomicInteger

object Scope {
	private val _uid: AtomicInteger = new AtomicInteger(0)

	def uid(): Int = _uid.incrementAndGet().toInt
}

case class Scope(parent: Scope, ns: Namespace, owner: Node) {
	// readonly id
	protected val _uid = Scope.uid()

	def uid = _uid

	// utility method for creating new scope/getting parent
	def enter(space: Namespace, owner: Node) = Scope(this, space, owner)

	def leave() = parent

	override def toString = s"${parent.toString}/$ns"
}

object RootScope extends Scope(null, NSRoot, TheProgram) {
	// root scope is uniquely identified by uid 0
	override val _uid = 0

	// ... you can check out any time you like, but you can neeeever leeeavee!
	override def leave() = this

	override def toString = s"/"
}
