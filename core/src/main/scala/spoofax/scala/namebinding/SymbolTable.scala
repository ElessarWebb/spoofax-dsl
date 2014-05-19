package spoofax.scala.namebinding

import spoofax.scala.ast._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.Tuple2

case class Index(
	definitions: Map[QualifiedName, Task] = Map(),
	resolutions: Seq[Task] = List()
) {
	def define(d: (QualifiedName, Task)) = Index(
		definitions + d,
		resolutions
	)

	def resolve(d: (Task)) = Index(
		definitions,
		d +: resolutions
	)

	def deferred = resolutions
}

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

	override def toString = s"${parent.toString}/$ns"
}
object RootScope extends Scope(null, NSRoot, TheProgram) {
	// root scope is uniquely identified by uid 0
	override val _uid = 0

	// ... you can check out any time you like, but you can neeeever leeeavee!
	override def leave() = this

	override def toString = s"/"
}

case class QualifiedName(scope: Scope, ns: Namespace, name: String) {
	override def toString = s"${scope.toString}/$ns:$name"
}

case class NameNotFoundException(qname: QualifiedName) extends Exception(
	s"Name '${qname.name}' not found"
)

/**
 * Immutable class representing a view on the index
 */
case class SymbolTable(
	index: Index = Index(),
	inScopes: Map[Namespace, Scope] = Map[Namespace, Scope]()
) {

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

	/**
	 * Get the fully qualified name for the name in the given namespace,
	 * assuming the current scope for `ns`
	 * @param ns
	 * @param name
	 */
	def get_qualified_name(ns: Namespace, name: String) = QualifiedName(
		inScopes.getOrElse(ns, RootScope),
		ns,
		name
	)

	def define(ns: Namespace, name: String, term: Term) = SymbolTable(
		index define Tuple2(get_qualified_name(ns, name), DefinesTask(term)),
		inScopes
	)

	def lexical_lookup(ns: Namespace, name: String) = {
		SymbolTable(
			index resolve LookupTask(ns, name, inScopes.getScope(ns)),
			inScopes
		)
	}

	def execute(): SymbolTable = {
		index.deferred.foreach(_.execute(index))
		this
	}

	override def toString = {
		val defs = index.definitions.map { case (qname, t) => s"${qname.toString} -> ${t}" }.foldLeft[String]("") {
			case (acc, it) => acc + "\n" + it
		}
		val deferred = index.deferred.map { _.toString }.foldLeft("") { case (acc, it) => acc + "\n" + it }

		s""" DEFINITIONS
			 | ===========
			 | $defs
			 |
			 | DEFERRED
			 | ========
			 | $deferred
		 """.stripMargin
	}
}

sealed abstract class Task {
	def execute(index: Index): Future[Unit]
}

/**
 * Task of which the result is computed in a later stage
 */
abstract class DeferredTask(promise: Promise[Term]) extends Task

/**
 * Task of which the result is known at the time of creation
 */
abstract class ImmediateTask(term: Term) extends Task {
	def execute(index: Index) = Future {}
}

/**
 * Lookup the definition of the given name
 */
case class LookupTask(
	ns: Namespace,
	name: String,
	scope: Scope,
	p: Promise[Term] = Promise[Term]()
) extends DeferredTask(p) {
	def execute(index: Index) = Future {
		val qname = QualifiedName(scope, ns, name)
		index.definitions.get(qname) match {
			case Some(DefinesTask(t)) => p.success(t)
			case _ => p.failure(NameNotFoundException(qname))
		}
	}
}

/**
 * Define this term
 */
case class DefinesTask(term: Term) extends ImmediateTask(term)
