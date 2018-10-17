package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  val tree1: Tree[Int] = Branch(
    Branch(
      Leaf(5),
      Branch(
        Leaf(6),
        Leaf(7))),
    Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)),
        Leaf(2)),
      Branch(
        Leaf(3),
        Leaf(3))))
  val tree2: Tree[Char] = Branch(Leaf('a'), Leaf('b'))
  val emptyTree: Tree[Int] = Leaf(10)

  "Tree" should "correct calculate size" in {
    Tree.size(tree1) should be (8)
    Tree.size(emptyTree) should be (1)
    Tree.size(tree2) should be (2)
  }
  it should "correct calculate depth" in {
    Tree.depth(tree1) should be (4)
    Tree.depth(tree2) should be (1)
    Tree.depth(emptyTree) should be (0)
  }
  it should "correct find max number" in {
    Tree.max(tree1) should be (7)
    Tree.max(emptyTree) should be (10)
  }
  it should "should correct apply the function to all elements" in {
    val multiplyCharA: Int => String = x => "A" * x
    val treeString1: Tree[String] = Branch(
      Branch(
        Leaf(multiplyCharA(5)),
        Branch(
          Leaf(multiplyCharA(6)),
          Leaf(multiplyCharA(7)))),
      Branch(
        Branch(
          Branch(
            Leaf(multiplyCharA(1)),
            Leaf(multiplyCharA(2))),
          Leaf(multiplyCharA(2))),
        Branch(
          Leaf(multiplyCharA(3)),
          Leaf(multiplyCharA(3)))))
    Tree.map(tree1)(multiplyCharA) == treeString1 should be (true)
    Tree.map(tree1)(x => multiplyCharA(x + 1)) == treeString1 should be (false)
  }
}
