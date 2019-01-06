package functional_programming

import cats.kernel.Semigroup

import scala.Option
import scala.collection.immutable

/*sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](x: A*): List[A] = {
    if(x.isEmpty) Nil
    else Cons(x.head, apply(x.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x + sum(xs)
    case Nil => 0
  }

  def product(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x * product(xs)
    case Cons(0, xs) => 0
    case Nil => 1
  }

  def tail[A](x: List[A]): List[A] = x match {
    case Nil => Nil
    case Cons(_, x) => x
  }

  def setHead[A](x: A, list: List[A]): List[A] = list match {
    case Nil => Cons(x, Nil)
    case Cons(_,z) => Cons(x, z)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def go(i: Int, subList: List[A]) : List[A] = subList match {
      case Nil => Nil
      case Cons(_, y) => if(i == n) subList else go(i + 1, y)
    }
    go(0, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,y) => if(f(x)) dropWhile(y, f) else y
  }

  def sumNew(x: List[Int]): Int = x match {
    case Nil => 0
    case Cons(x, y) => x + sumNew(y)
  }

  def prodNew(x: List[Double]): Double = x match {
    case Nil => 0
    case Cons(x, y) => x * prodNew(y)
  }

  def foldRight[A, B](a: List[A], b: B)(f: (A, B) => B): B = a match {
    case Nil => b
    case Cons(p, r) => f(p, foldRight(r, b)(f))
  }
}
*/
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for{
      a <- this
      b1 <- b
    } yield f(a, b1)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object TestObj extends App{

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if(xs.isEmpty) Left("No value")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x/y)
    catch{
      case e: Exception => Left(e)
    }
  }

  def parseInsuranceRateQuote( age: String, numberOfSpeedingTickets: String): Either[Exception,Double] = {
    for{
      age <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(age, tickets)
  }


  def traverse[E,A,B](es: scala.collection.immutable.List[A])(f: A => Either[E, B]): Either[E, scala.collection.immutable.List[B]] =  es match {
    case immutable.Nil => Right(immutable.Nil)
    case a::b => f(a) flatMap(x => traverse(b)(f) map(x::_))
  }


  val list = scala.collection.immutable.List(1, 2, 3)
  println(traverse(list)(x => Right(x + "asdf")))

  def insuranceRateQuote(a: Int, tickets: Int): Int = {
    val p1 = scala.collection.immutable.List("Kim")
    "asdf"::p1
    1012
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  import cats.implicits._
  val aMap = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combineMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
  println(combineMap.get("foo"))

  import io.circe.optics.JsonPath._
  import io.circe._, io.circe.parser._

  val _address = root.order.customer.contactDetails.address.string
  val json: Json = parse("").getOrElse(Json.Null)

  //val address: Option[String] = _address.getOption(json)
}

