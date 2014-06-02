import com.sun.corba.se.spi.orbutil.threadpool.ThreadPoolChooser
import java.util.concurrent.ThreadPoolExecutor
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.AsyncAssertions._
import org.scalatest.junit.JUnitRunner
import org.scalatest.time.SpanSugar._
import org.junit.runner.RunWith

import scala.collection.mutable.Buffer
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.util.{Success, Failure}
import spoofax.scala.ast.{Leaf, Node}
import spoofax.scala.namebinding._

@RunWith(classOf[JUnitRunner])
class SimpleNamerSpec extends FlatSpec with Matchers {
	case object NSClass extends Namespace

	case class Module(classes: List[Class]) extends Node() {
		def children = classes
	}
	case class Class(name: String, parent: Option[ID]) extends Node() {
		def children = parent.map(List(_)).getOrElse(Nil)
	}
	case class ID(name: String) extends Node() with Leaf

	val ca = Class("A", None)
	val cb = Class("B", Some(ID("A")))

	val ast = Module(List(ca, cb))

	/**
	 * fixture for a test that does some work as a future.
	 * sets up the execution context and a waiter.
	 *
	 * Takes the number of dismissals as an argument.
	 */
	def parallel[T](dis: Int)(test: (Waiter, ExecutionContext) => T) = {
		val pool = Executors.newFixedThreadPool(8)
		val waiter = new Waiter()
		test(waiter, ExecutionContext.fromExecutor(pool))
		waiter.await(timeout(3 seconds), dismissals(dis))
		pool.shutdown()
	}

	"namer" should "resolve create appropriate lookuptask" in parallel(0) { (w, ctx) =>
		val definitions: NamingRules = context => {
			import context._

			// actual rules
			{
				case Class(x, Some(parent@ID(pname))) => defines(NSClass, x) and parent.references(NSClass, pname)
				case Class(x, _) => defines(NSClass, x)
			}
		}

		val (_, tab) = Namer(definitions)(ast)(ctx)

		// tasks are created synchronously
		val alookups = tab.index.deferred.filter {case LookupTask(NSClass, "A", _) => true}
		assert(alookups.length > 0, "expected that a lookuptask for class A was created")
		assert(alookups.length == 1, "expected just one lookuptask for class A")
	}
}
