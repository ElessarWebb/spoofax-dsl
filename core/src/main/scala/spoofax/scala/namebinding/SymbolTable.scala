package spoofax.scala.namebinding

import spoofax.scala.ast._
import scala.concurrent._
import scala.util.Success
import rx.lang.scala._
import rx.lang.scala.subjects.BehaviorSubject

case class Index(
	definitions: Map[QualifiedName, Task[Node]] = Map(),
	resolutions: Seq[DeferredTask[_]] = List()
) {
	def define(d: (QualifiedName, Task[Node])) = Index(
		definitions + d,
		resolutions
	)

	def resolve(d: (DeferredTask[_])) = Index(
		definitions,
		d +: resolutions
	)

	def deferred = resolutions
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

	def enter_scope(scopes: List[Namespace], owner: Node) = SymbolTable(
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

	def define(ns: Namespace, name: String, term: Node): (Task[Node], SymbolTable)= {
		val task = DefinesTask(term)
		(task, SymbolTable(
			index define Tuple2(get_qualified_name(ns, name), task),
			inScopes
		))
	}

	def lexical_lookup(ns: Namespace, name: String): (Task[Option[Node]], SymbolTable) = {
		val task = LookupTask(ns, name, inScopes.getScope(ns))
		(task, SymbolTable(
			index resolve task,
			inScopes
		))
	}

	def execute()(implicit ect: ExecutionContext): SymbolTable = {
		index.deferred.foreach(_.exec(index))
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

sealed abstract class Task[T] {
	def result: Observable[T]
}

/**
 * Task of which the result is computed in a later stage
 */
abstract class DeferredTask[T] extends Task[T] {
	def exec(index: Index)(implicit ectx: ExecutionContext): Observable[T]
}

/**
 * Task of which the result is known at the time of creation
 * and is immutable
 */
abstract class ImmediateTask[T](value: T) extends Task[T] {
	// immediately fulfill the promise
	override def result = Observable.items(value)
}

/**
 * Lookup the definition of the given name
 */
case class LookupTask(ns: Namespace, name: String, scope: Scope) extends DeferredTask[Option[Node]] {
	val _result = BehaviorSubject[Option[Node]](None)

	def result: Observable[Option[Node]] = _result

	def exec(index: Index)(implicit ectx: ExecutionContext) = {
		Future {
			val qname = QualifiedName(scope, ns, name)
			index.definitions.get(qname) match {
				case Some(DefinesTask(t)) =>
					_result.onNext(Some(t))
				case _ =>
					_result.onNext(None)
					_result.onError(NameNotFoundException(qname))
			}
		}

		result
	}
}

/**
 * Define this term
 */
case class DefinesTask(term: Node) extends ImmediateTask(term)
