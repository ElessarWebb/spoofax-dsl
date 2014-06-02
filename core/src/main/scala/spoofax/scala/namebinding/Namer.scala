package spoofax.scala

import spoofax.scala.ast._

package object namebinding {
	type SymTrans = SymbolTable => SymbolTable

	// naming rules are rules that take a rule context and return a
	// partial function from terms to a transformation of the context.
	// this laziness allows for flexible composability of the rules
	// and 'behind the scenes' scoping of the rules to the current terms
	type NamingRules = (RuleContext => NamingRule)
	type NamingRule = PartialFunction[Node, SymTrans]

	case class QualifiedName(scope: Scope, ns: Namespace, name: String) {
		override def toString = s"${scope.toString}/$ns:$name"
	}

	/**
	 * Composition functions for context transformations
	 */
	implicit class SymTransComposers(st: SymTrans) {
		def and(f: SymTrans): SymTrans = { tab => f(st(tab)) }
	}

	object SymTrans {
		def define(term: Node, ns: Namespace, name: String): SymTrans = { symtab =>
			val (task, table) = symtab.define(ns, name, term)
			table
		}

		def references(term: Node, ns: Namespace, name: String): SymTrans = { symtab =>
			val (task, table) = symtab.lexical_lookup(ns, name)
			table
		}

		def leave_scope(scopes: List[Namespace]): SymTrans = { symtab =>
			symtab.leave_scope(scopes)
		}

		def enter_scope(scopes: List[Namespace], owner: Node): SymTrans = { symtab =>
			symtab.enter_scope(scopes, owner)
		}
	}
}

package namebinding {

import scala.concurrent.ExecutionContext

class Identifier(name: String, lookup: Option[LookupTask] = None) extends Node() with Leaf

abstract class RuleContext(
	currentTerm: Node
) {
	implicit def onTerm(term: Node): RuleContext

	def defines(ns: Namespace, name: String): SymTrans
	def references(ns: Namespace, name: String): SymTrans
	def scopes(scopes: List[Namespace]): SymTrans
}

case class OnEnterContext(
	currentTerm: Node
) extends RuleContext(currentTerm) {
	implicit def onTerm(term: Node) = OnEnterContext(term)

	def defines(ns: Namespace, name: String): SymTrans = SymTrans.define(currentTerm, ns, name)
	def references(ns: Namespace, name: String): SymTrans = SymTrans.references(currentTerm, ns, name)
	def scopes(scopes: List[Namespace]): SymTrans = SymTrans.enter_scope(scopes, currentTerm)
}

case class OnLeaveContext(
	currentTerm: Node
) extends RuleContext(currentTerm) {
	implicit def onTerm(term: Node) = OnLeaveContext(term)

	override def defines(ns: Namespace, name: String): SymTrans = identity
	override def references(ns: Namespace, name: String): SymTrans = identity
	override def scopes(scopes: List[Namespace]): SymTrans = SymTrans.leave_scope(scopes)
}

/**
 * Container for a set of namebinding rules.
 * Apply it to an AST to get back an annotated AST with the corresponding symboltable
 *
 * @param rules Namebinding rules
 */
case class Namer(rules: NamingRules) {

	/** Pass 1: collect all tasks on the AST
		*/
	def collect(ast: AST): SymbolTable = ast.topdown(SymbolTable())({
		// rules are partial functions from terms to (SymTab => Symtab)
		// if the term is not part of the domain of the rules,
		// return the identity function w.r.t. the symboltable

		// on enter we scope, define and reference
		case (symtab, current) =>
			rules(OnEnterContext(current)).applyOrElse(current, {
				_: Node => _: SymbolTable => symtab
			})(symtab)
	}, {
		// on leave, we leave scopes ^^
		case (symtab, current) =>
			rules(OnLeaveContext(current)).applyOrElse(current, {
				_: Node => _: SymbolTable => symtab
			})(symtab)
	})

	def execute(symtab: SymbolTable)(implicit ectx: ExecutionContext): SymbolTable = symtab.execute()

	def apply(ast: AST)(implicit ectx: ExecutionContext): (AST, SymbolTable) = (ast, (collect _ andThen execute)(ast))
}
}
