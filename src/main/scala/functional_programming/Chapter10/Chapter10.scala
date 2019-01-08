package functional_programming.Chapter10

import functional_programming.Chapter7.Monoid

/**
  * Created by gouthamvidyapradhan on 08/01/2019
  */
trait Monoid[A] {
  def op(op1: A, op2: A): A
  def zero: A
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object Chapter10 extends App {

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Nil    => z
        case a :: b => f(a, foldRight(b)(z)(f))
      }

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Nil    => z
        case a :: b => foldLeft(b)(f(z, a))(f)
      }

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
      foldLeft(as.map(f))(mb.zero)(mb.op)
    }
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(op1: Int, op2: Int): Int = op1 + op2
    override def zero: Int = 0
  }

  val stringMonoid = new Monoid[String] {
    override def op(op1: String, op2: String): String = {
      op1 + op2
    }
    override def zero: String = {
      ""
    }
  }

  println(foldableList.foldMap(List(1, 2, 3))(x => x.toString)(stringMonoid))
}
