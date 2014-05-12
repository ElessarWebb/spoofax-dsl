import org.scalatest._
import spoofax.scala.macros.term
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TermMacroSpec extends FlatSpec with Matchers {

	"the macro" should "maintain existing body while expanding" in {
		@term case class A(x: Int) {
			def existing() = x + 40
		}
		val a = A(2)

		a.existing() should be (42)
		A.companion(10) should be (42)
	}
}
