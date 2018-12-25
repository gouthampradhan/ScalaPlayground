package functional_programming

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
}

