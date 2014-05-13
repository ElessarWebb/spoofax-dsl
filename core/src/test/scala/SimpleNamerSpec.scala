import org.scalatest.{FlatSpec, Matchers}

import spoofax.scala.ast.Term
import spoofax.scala.namebinding.{NamingRules, Namespace, Namer}

case object NSClass extends Namespace

case class Module(classes: List[Class]) extends Term(classes)
case class Class(name: String, parent: Option[ID]) extends Term(parent.map(List(_)).getOrElse(Nil))
case class ID(name: String) extends Term(Nil)

class SimpleNamerSpec extends FlatSpec with Matchers {

	val ca = Class("A", None)
	val cb = Class("B", Some(ID("A")))

	val ast = Module(List(ca, cb))

	"namer" should "resolve create definitions" in {
		val definitions: NamingRules = context => {
			import context._

			// actual rules
			{
				case Class(x, _) => defines(NSClass, x)
			}
		}

		val (_, tab) = Namer(definitions)(ast)

		assert(tab.global.symbols(Tuple2(NSClass, "A")) == ca)
		assert(tab.global.symbols(Tuple2(NSClass, "B")) == cb)
	}

	"namer" should "resolve backward references in same scope" in {
		val definitions: NamingRules = context => {
			import context._

			// actual rules
			{
				case c@Class(x, p) =>
					defines(NSClass, x) and references(NSClass, x)
			}
		}

		val (_, tab) = Namer(definitions)(ast)
	}
}
