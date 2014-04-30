import org.scalatest._
import spoofax.scala.ast._

class TermsSpec extends FlatSpec with Matchers {

	case class Node(i: Int, _children: List[Node] = List()) extends Term[Nothing]((0,0)) {
		override def toString = s"$i"
		override def children = _children
	}

	val ast = Node(0, List(Node(1, List(Node(3), Node(4))), Node(2, List(Node(5), Node(6)))))

	"foldPreOrder" should "treat a node first and then it's children from left to right recursively" in {
		ast.foldDown("") { case (acc, n) => acc + n } should be ("0134256")
	}

	"foldPostOrder" should "treat a node's children first from left to right and then itself recursively" in {
		ast.foldUp("") { case (acc, n) => acc + n } should be ("3415620")
	}
}
