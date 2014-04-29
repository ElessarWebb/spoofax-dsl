import org.scalatest._
import spoofax.scala.ast._

class TermsSpec extends FlatSpec with Matchers {

	case class Node(i: Int, _children: List[Node] = List()) extends Term {
		override def toString = s"$i"
		override def children = _children
	}

	val ast = Node(0, List(Node(1), Node(2)))

	"FoldDown" should "treat a node first and then it's children from left to right" in {
		ast.foldDown("") { case (acc, n) => acc + n } should be ("012")
	}

	"FoldUp" should "treat a node's children first from left to right and then itself" in {
		ast.foldUp("") { case (acc, n) => acc + n } should be ("120")
	}
}
