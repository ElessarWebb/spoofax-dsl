import org.scalatest.{FlatSpec, Matchers}

import spoofax.scala.ast.Term
import spoofax.scala.namebinding.{Rules, Namespace, Namer}

case object NSClass extends Namespace

case class Module(classes: List[Class]) extends Term(classes)
case class Class(name: String, parent: Option[String]) extends Term(Nil)

class SimpleNamerSpec extends FlatSpec with Matchers {

	val ca = Class("A", None)
	val cb = Class("B", Some("A"))

	val ast = Module(List(ca, cb))

	"namer" should "resolve create definitions" in {
		val definitions: Rules = context => {
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
}
