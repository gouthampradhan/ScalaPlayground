package cats

import cats.data.EitherT
import cats.implicits._

import scala.collection.immutable.List
import scala.concurrent.Future
import scala.util.Try

object CatsTest extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val aMap = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combineMap =
    Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
  println(combineMap.get("foo"))

  println(Monoid[String].empty)
  println(Monoid[String].combineAll(List("a", "b", "c")))
  println(Monoid[String].combineAll(List()))

  import Helper._

  5.times(println("Hi"))

  def parseDouble(s: String): Either[String, Double] = {
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))
  }

  def divide(a: Double, b: Double): Either[String, Double] = {
    Either.cond(b != 0, a / b, "Cant divide by zero")
  }

  def divisionProgram(a: String, b: String): Either[String, Double] = {
    for {
      doubleA <- parseDouble(a)
      doubleB <- parseDouble(b)
      result  <- divide(doubleA, doubleB)
    } yield result
  }

  def parseDoubleAsync(s: String): Future[Either[String, Double]] = {
    Future.successful(parseDouble(s))
  }

  def divideAsync(a: Double, b: Double): Future[Either[String, Double]] = {
    Future.successful(divide(a, b))
  }

  def divisionProgramAsync(a: String,
                           b: String): EitherT[Future, String, Double] =
    for{
      a <- EitherT(parseDoubleAsync(a))
      b <- EitherT(parseDoubleAsync(b))
      c <- EitherT(divideAsync(a, b))
    } yield c

  val number: EitherT[Option, String, Int] = EitherT.rightT(5)
  val error: EitherT[Option, String, Int] = EitherT.leftT("error")

  val numberO: Option[Int] = Some(5)
  val number1 = EitherT.right(numberO)
  //val number1 = EitherT.left(Some(4))

  val numberE: Either[String, Int] = Right(56)
  val errorE: Either[String, Int] = Left("Error")
  val numberFE: List[Either[String, Int]] = List(Right(4), Right(5))

  val numberT: EitherT[List, String, Int] = EitherT.fromEither(numberE)
  val numberTE: EitherT[List, String, Int] = EitherT(numberFE)
  println(numberTE)

  //List(Right(4), Right(5)).sequence
  //List(1, 2, 4).sequence
  import cats.implicits._
  val list: List[List[Int]] = List(List(1, 2), List(2), List(4))
  val sequenced = list.sequence
  println(s"result is $sequenced")
}
