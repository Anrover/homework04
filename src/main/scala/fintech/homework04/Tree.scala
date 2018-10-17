package fintech.homework04
import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Branching extends Tree[Nothing]

object Tree {
  // реализовать функцию fold

//not tailrec
//  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
//    case Leaf(value) => f(value)
//    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
//  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    @tailrec
    def rec(subtrees: List[Tree[A]], acc: List[B]): B = subtrees match {
      case List() => acc.head
      case head :: tail => head match {
        case Leaf(value) => rec(tail, f(value) :: acc)
        case Branch(left, right) => rec(left :: right :: Branching :: tail, acc)
        case Branching => rec(tail, g(acc.tail.head, acc.head) :: acc.drop(2))
      }
    }
    rec(List(t), List())
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => x + y)

  def max(t: Tree[Int]): Int = fold(t)(x => x)((x, y) => Math.max(x, y))

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 0)((x, y) => 1 + Math.max(x, y))

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
}
