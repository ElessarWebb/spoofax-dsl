package spoofax.scala.namebinding

/**
 * The Renamer Monad[SymbolTable] is concerned with resolving all identifiers in the source
 * to their respective terms
 */
case class Renamer {
	def flatMap(st => Renamer)
}