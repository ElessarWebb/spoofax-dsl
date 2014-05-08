package spoofax.scala

import spoofax.scala.ast.{AST, Term}

package object namebinding {
	type Rules = (RuleContext => PartialFunction[Term, SymbolTable])
}

package namebinding {

	case class RuleContext(symtab: SymbolTable, currentTerm: Term) {
		def defines(ns: Namespace, name: String) = symtab.define(ns, name, currentTerm)
	}

	case class Namer(rules: Rules) {

		def apply(ast: AST): (AST, SymbolTable) = (ast, ast.foldDown(SymbolTable()) {
			case (symtab, term) => rules(RuleContext(symtab, term)).applyOrElse(term, {_: Term => symtab})
		})
	}
}
