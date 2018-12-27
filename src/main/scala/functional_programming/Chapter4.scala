package functional_programming

import scala.reflect.internal.Trees
import scala.util.Try

/**
  * Created by gouthamvidyapradhan on 22/12/2018
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if(f(a)) Some(a) else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Test extends App{
  val op1: Option[Int] = Some(13)
  val op2: Option[Int] = None
  println(op1.map(_.toString).getOrElse("This is a default String"))
  println(op2.map(_.toString).getOrElse("This is a default String"))

  println(s"Filter if the number is odd: ${op1.filter(x => x % 2 != 0).getOrElse("Number is even")}")

  def variance(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else{
      val mean = xs.sum / xs.size
      val variance = xs.map(x => Math.pow(x - mean, 2)).sum / xs.size
      Some(variance)
    }
  }

  println(s"Varience is ${variance(Seq()).flatMap(x => Some(x))}")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {age.toInt}
    val optNoSpeedingTickets: Option[Int] = Try {numberOfSpeedingTickets.toInt}
    map2(optAge, optNoSpeedingTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Int = {
    //dummy value
    1750
  }

  println(s"Result of map2:${map2(Some(1), Some(2))(insuranceRateQuote)}")

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch{
      case e: Exception => None
    }
  }

  /*def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], list: List[A]): Option[List[A]] = a match {
      case Nil => Some(list)

      case Cons(x, y) => x match {
        case None => None
        case Some(a) => go(y, Cons(a, list))
      }
    }
    go(a, List())
  }*/

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], list: List[A]): Option[List[A]] = a match {
    case Nil => Some(list)
    case Cons(x, y) => x.flatMap(z => go(y, Cons(z, list)))
    }
    go(a, List())
  }

  val list = List(Some(1), Some(2), Some(3))
  val result = sequence(list)
  println(result.getOrElse(List("Empty List")))


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(List.map(a)(f))
  }
}


