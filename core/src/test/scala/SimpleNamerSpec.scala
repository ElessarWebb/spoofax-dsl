import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import spoofax.scala.ast.{Leaf, Term}
import spoofax.scala.namebinding._

@RunWith(classOf[JUnitRunner])
class SimpleNamerSpec extends FlatSpec with Matchers {
	case object NSClass extends Namespace

	case class Module(classes: List[Class]) extends Term {
		def children = classes
	}
	case class Class(name: String, parent: Option[ID]) extends Term {
		def children = parent.map(List(_)).getOrElse(Nil)
	}
	case class ID(name: String) extends Term with Leaf

	val ca = Class("A", None)
	val cb = Class("B", Some(ID("A")))

	val ast = Module(List(ca, cb))

	"namer" should "resolve create definitions" in {
		val definitions: NamingRules = context => {
			import context._

			// actual rules
			{
				case Class(x, Some(parent@ID(pname))) => defines(NSClass, x) and parent.references(NSClass, pname)
				case Class(x, _) => defines(NSClass, x)
			}
		}

		val (_, tab) = Namer(definitions)(ast)

		println(tab)
	}

	it should "resolve backward references in same scope" in {
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
