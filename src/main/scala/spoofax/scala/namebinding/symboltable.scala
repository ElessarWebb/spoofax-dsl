package spoofax.scala.namebinding

import spoofax.scala.ast._
import scala.concurrent.Future
import scala.collection.immutable.MapLike
import scala.collection

/**
 * @param parent
 * @param symbols
 */
case class SymbolTable(
	  parent: Option[SymbolTable],
		symbols: Map[(Namespace, String), Term[_]])
{
	def lookup_local(ns: Namespace, id: String): Future[Term[_]] = ???
	def lookup_lexical(ns: Namespace, id: String): Future[Term[_]] = ???
	def lookup_surrounding(ns: Namespace, id: String): Future[Term[_]] = ???
}
