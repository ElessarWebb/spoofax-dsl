import org.scalatest._
import spoofax.scala.macros.Term

class TermMacroSpec extends FlatSpec with Matchers {

	"the macro" should "expand to return 42" in {
		Term() should be (42)
	}
}
