package spoofax.scala.namebinding

import rx.lang.scala.Observable
import spoofax.scala.ast._
import scala.collection.immutable.HashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent._
import scala.util.Try
import scala.concurrent.duration.Duration
import scala.Tuple2

object Scope {
	private val _uid: AtomicInteger = new AtomicInteger(0)
	def uid(): Int = _uid.incrementAndGet().toInt
}

case class Scope(parent: Scope, ns: Namespace, owner: Term) {
	// readonly id
	protected val _uid = Scope.uid()
	def uid = _uid

	// utility method for creating new scope/getting parent
	def enter(space: Namespace, owner: Term) = Scope(this, space, owner)
	def leave() = parent
}
object RootScope extends Scope(null, NSRoot, TheProgram) {
	// root scope is uniquely identified by uid 0
	override val _uid = 0

	// ... you can check out any time you like, but you can neeeever leeeavee!
	override def leave() = this
}

object SymbolTable {
	def apply(index: Index, inScopes: Map[Namespace, Scope])(implicit todos: Seq[Task]) =
		SymbolTable(index, inScopes, todos)
}

/**
 * Immutable class representing a view on the index
 */
case class SymbolTable(
	index: Index = Map[Scope, Observable[Term]](),
	inScopes: Map[Namespace, Scope] = Map[Namespace, Scope](),
	implicit val todos: Seq[Task] = Nil
)(implicit ec: ExecutionContext) {

	implicit class ScopesWrapper(val scopes: Map[Namespace, Scope]) {
		def getScope(ns: Namespace) = scopes.getOrElse(ns, RootScope)
	}

	def enter_scope(scopes: List[Namespace], owner: Term) = SymbolTable(
		index,
		scopes.foldLeft(inScopes) {
			case (in, ns) => in.updated(
				ns,
				in.getScope(ns).enter(ns, owner)
			)
		}
	)

	def leave_scope(scopes: List[Namespace]) = SymbolTable(
		index,
		scopes.foldLeft(inScopes) {
			case (in, ns) => in.updated(ns, in.getScope(ns).leave())
		}
	)

	def define(ns: Namespace, name: String, term: Term) = SymbolTable(
		index + Tuple2(inScopes.getOrElse(ns, RootScope), Observable.items(term)),
		inScopes
	)

	def lexical_lookup(ns: Namespace, name: String) = {
		val lookuptask = LookupTask(ns, name)
		SymbolTable(
			index + Tuple2(inScopes.getScope(ns), lookuptask.observable),
			inScopes,
			lookuptask +: todos
		)
	}
}

sealed class Task(promise: Promise[Term]) {
	def observable: Observable[Term] = Observable.from(promise.future)
}

case class LookupTask(ns: Namespace, name: String, p: Promise[Term] = Promise[Term]()) extends Task(p)
