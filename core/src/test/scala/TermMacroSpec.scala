import org.scalatest._
import spoofax.scala.macros.term
import spoofax.scala.ast._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TermMacroSpec extends FlatSpec with Matchers {

	case class IntTerm(x: Int) extends Term with Leaf {
		override def toString = s"$x"
	}

	@term case class A(x: Term, y: Term) extends Term {
		def existing() = x
	}

	val x = IntTerm(4)
	val y = IntTerm(2)

	val inst = A(x, y)

	"the macro" should "expand to a term without losing custom definitions" in {
		// existing defintions should survive
		inst.existing() should be(x)
	}

	it should "implement the children function properly" in {
		// children method should be implemented by macro
		inst.children() should be(Seq(x, y))
	}

	it should "implement apply on the companion object with the original term arguments" in {
		// apply of case class should survive
		A(x, y).children() should be(Seq(x, y))
	}

	it should "implement an unapply with the original term arguments" in {
		// macro unapply
		inst match {
			case A(x, y) => (x,y) should be (3, 4)
		}
	}
}
