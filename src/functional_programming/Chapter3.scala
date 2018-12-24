package functional_programming

/**
  * Created by gouthamvidyapradhan on 22/12/2018
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
}

object Chapter3 extends App{
  val sum = List.foldRight(List(1, 2, 3, 4), 0)(_ + _)
  val product1 = List.foldRight(List(1, 2, 3, 4), 1.0)(_ * _)
  val product2 = List.foldLeft(List(1, 2, 3, 4), 1.0)(_ * _)
  println(s"Sum is ${sum}")
  println(s"Product from foldRight is ${product1}")
  println(s"Product from foldLeft is ${product2}")
}
