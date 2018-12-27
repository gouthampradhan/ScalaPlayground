package functional_programming

/**
  * Created by gouthamvidyapradhan on 22/12/2018
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//companion object
object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, y) => x + sum(y)
  }

  def product(ints: List[Int]): Double = ints match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, y) => x * product(y)
  }

  def foldRight[A, B](a: List[A], b: B)(f: (A, B) => B): B = a match {
    case Nil => b
    case Cons(x, xs) => f(x, foldRight(xs, b)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: List[A], head: List[A])(f: (A, List[A]) => List[A]): List[A] = as match {
    case Nil => head
    case Cons(x, xs) => reverse(xs, f(x, head))(f)
  }

  def merge[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, y) => Cons(x, merge(y, l2))
  }

  def mergeList[A](as: List[List[A]], a: List[A])(f:(List[A], List[A]) => List[A]): List[A] = as match {
    case Nil => a
    case Cons(x, y) => f(x, mergeList(y, a)(f))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, y) => Cons(f(x), map(y)(f))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, y) => merge(f(x), flatMap(y)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, y) =>
      if(f(x)) {
        filter(y)(f)
      } else {
        Cons(x, filter(y)(f))
      }
  }

  def sumList[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = l1 match {
    case Nil => Nil
    case Cons(a, b) => {
      l2 match {
        case Cons(c, d) => {
          Cons(f(a, c), sumList(b, d)(f))
        }
      }
    }
  }
}

object Tree {
    def count[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => count(l) + count(r) + 1
    }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def maxDepth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => maxDepth(l).max(maxDepth(r)) + 1
  }

  def fold[A](t: Tree[A], a: A)(f:(A, A) => A): A = t match {
    case Leaf(_) => a
    case Branch(l, r) => f(fold(l, a)(f), fold(r, a)(f))
  }
}

object Chapter3 extends App{
  val sum = List.foldRight(List(1, 2, 3, 4), 0)(_ + _)
  println(s"Sum is ${sum}")

  val product1 = List.foldRight(List(1, 2, 3, 4), 1.0)(_ * _)
  println(s"Product from foldRight is ${product1}")

  val product2 = List.foldLeft(List(1, 2, 3, 4), 1.0)(_ * _)
  println(s"Product from foldLeft is ${product2}")

  val sum2 = List.foldLeft(List(1, 2, 3, 4), 0)(_ + _)
  println(s"Sum is ${sum2}")

  val length = List.foldLeft(List(1, 2, 3, 4), 0)((x, _) => x + 1)
  println(s"Length of list is ${length}")

  val reversedList = List.reverse(List(1, 2, 3, 4), Nil)((a, b) => Cons(a, b))
  println(s"List after reversing: $reversedList")

  val append = List.foldLeft(List("1", "2", "3", "4"), "")((x, y) => x + y)
  println(s"result of append is:  ${append}")

  val mergeListResult = List.mergeList(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10, 11)), Nil)(List.merge)
  println(s"Merged list:  ${mergeListResult}")

  val mapResult = List.map(List(1, 2, 3))(x => "StringValue: " + x.toString)
  println(s"Map result:  ${mapResult}")

  val oddNumbers = List.filter(List(1, 2, 3, 4, 5, 6, 7, 7))(x => (x % 2) == 0)
  println(s"Map result:  ${oddNumbers}")

  val sumOfTwoList = List.sumList(List(1, 2, 3), List(4, 5, 5))((x, y) => x + y)
  println(s"Sum of two lists:  ${sumOfTwoList}")

}

