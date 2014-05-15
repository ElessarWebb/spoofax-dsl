package spoofax.scala.namebinding

import spoofax.scala.ast._
import scala.concurrent.Future
import scala.collection.immutable
import scala.collection.mutable

protected case class Scope(
	parent: Option[Scope],
	scopes: Seq[Namespace],
	symbols: mutable.Map[(Namespace, String), Term] = new mutable.HashMap()
	) {

	def define(ns: Namespace, name: String, term: Term) = {
		symbols += Tuple2((ns, name), term)
		this
	}

	// scoping
	def enter_scope(scopes: Seq[Namespace]) = Scope(Some(this), scopes)

	def leave_scope(): Option[Scope] = parent

	/**
	 * Lookup a name in this scope
	 */
	private def lookup_local(ns: Namespace, id: String): Option[Term] = symbols.get((ns, id))

	/**
	 * Lookup a name in the parent scope
	 */
	private def lookup_parent(ns: Namespace, id: String): Option[Term] = parent.flatMap { ps =>
		ps.lookup_lexical(ns, id)
	}

	/**
	 * Lookup a name in the lexical scope for the given namespace
	 */
	def lookup_lexical(ns: Namespace, id: String): Option[Term] = lookup_local(ns, id).orElse {
		if(!scopes.contains(ns)) lookup_parent(ns, id)
		else None
	}

}

object SymbolTable {
	def apply(): SymbolTable = {
		val global = Scope(None, List())
		SymbolTable(global, global)
	}
}

case class SymbolTable(
	global: Scope,
	current: Scope,
	term_scopes: Map[Term, Scope] = new immutable.HashMap()
	) {

	def define(ns: Namespace, name: String, term: Term) = SymbolTable(
		global,
		current.define(ns, name, term),
		term_scopes
	)

	def enter_scope(owner: Term, scopes: Seq[Namespace]): SymbolTable = {
		val scope = current.enter_scope(scopes)
		SymbolTable(global, scope, term_scopes + Tuple2(owner, scope))
	}

	def leave_scope(): Option[SymbolTable] = current.parent.map(SymbolTable(global, _, term_scopes))

	def lookup_lexical(ns: Namespace, name: String) = current.lookup_lexical(ns, name)
}
