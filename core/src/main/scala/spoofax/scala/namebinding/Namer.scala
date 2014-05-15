package spoofax.scala

import spoofax.scala.ast.{AST, Term}

package object namebinding {
	// type of functions that transform the symboltable in any sense
	type SymTrans = SymbolTable => SymbolTable

	// naming rules are rules that take a rule context and return a
	// partial function from terms to a transformation of the symboltable
	// this laziness allows for flexible composability of the rules
	// and 'behind the scenes' scoping of the rules to the current terms
	type NamingRules = (RuleContext => NamingRule)
	type NamingRule = PartialFunction[Term, SymTrans]

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
		def references(ns: Namespace, name: String): SymTrans = SymTrans.references(ns, name)
	}

	/**
	 * Composition functions for symboltable  transformations
	 */
	implicit class SymTransComposers(st: SymTrans) {
		def and(f: SymTrans): SymTrans = { tab => f(st(tab)) }
	}

	object SymTrans {
		def references(ns: Namespace, name: String) = { symtab: SymbolTable =>
			val definition = symtab.lookup_lexical(ns, name)
			// TODO bind it to the ast

			symtab
		}
	}
}

package namebinding {

	case class RuleContext(currentTerm: Term) {
		def defines(ns: Namespace, name: String): SymTrans = _.define(ns, name, currentTerm)
		def references(ns: Namespace, name: String): SymTrans = SymTrans.references(ns, name)
		def scopes(scopes: List[Namespace]): SymTrans = _.enter_scope(currentTerm, scopes)
	}

	/**
	 * Container for a set of namebinding rules.
	 * Apply it to an AST to get back an annotated AST with the corresponding symboltable
	 *
	 * @param rules Namebinding rules
	 */
	case class Namer(rules: NamingRules) {

		def apply(ast: AST): (AST, SymbolTable) = (ast, ast.foldDown(SymbolTable()) {
			case (symtab, term) =>
				// rules are partial functions from terms to (SymTab => Symtab)
				// if the term is not part of the domain of the rule,
				// return the identity function w.r.t. the symboltable
				rules(RuleContext(term)).applyOrElse(term, {
					_: Term => _: SymbolTable => symtab
				})(symtab)
		})
	}
}
