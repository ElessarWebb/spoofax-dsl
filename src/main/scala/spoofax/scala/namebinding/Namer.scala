package spoofax.scala

import spoofax.scala.ast.{AST, Term}

package object namebinding {
	type NamingRules = (RuleContext => NamingRule)
	type NamingRule = PartialFunction[Term, SymTrans]
	type SymTrans = SymbolTable => SymbolTable

	/**
	 * This implicit class allows one to write namebinding rules on sub terms of matched terms.
	 * e.g.:
	 *
	 * case Class(x, Some(p@ID(pname))) =>
	 * 				defines(NSClass, x)
	 * 		and p.references(NSClass, pname)
	 */
	implicit class OnTerm[T <: Term](val term: T) extends AnyVal {
		def defines(ns: Namespace, name: String): SymTrans = _.define(ns, name, term)
		def references(ns: Namespace, name: String): SymTrans = _.lookup_lexical(ns, name)
	}

	/**
	 * Makes it possible to have multiple nab rules on one matched term
	 */
	implicit class SymTransComposers(st: SymTrans) {
		def and(f: SymTrans): SymTrans = { tab => f(st(tab)) }
	}
}

package namebinding {

	case class RuleContext(currentTerm: Term) {
		import RuleContext._

		def defines(ns: Namespace, name: String): SymTrans = _.define(ns, name, currentTerm)
		def references(ns: Namespace, name: String): SymTrans = _.lookup_lexical(ns, name)
	}

	case class Namer(rules: NamingRules) {

		def apply(ast: AST): (AST, SymbolTable) = (ast, ast.foldDown(SymbolTable()) {
			case (symtab, term) =>
				rules(RuleContext(term)).applyOrElse(term, {
					_: Term => _: SymbolTable => symtab
				})(symtab)
		})
	}
}
