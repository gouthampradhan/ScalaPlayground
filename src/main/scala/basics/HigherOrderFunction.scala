package basics

/**
  * Created by gouthamvidyapradhan on 01/11/2017.
  */
object HigherOrderFunction extends App{

  //method
  def charAt(s: String, i: Int) : Char = {
    s.charAt(i)
  }

  //function
  val f: (String, Int) => Char = _.charAt(_)
  println(f("Gouthamn", 1))

  val f1 = (_:String).charAt(_:Int)
  println(f1.apply("Goutham", 1))

  import scala.math._
  val fun = ceil _

  //map function
  Array(1.2, 1.3, 0.23).map(fun).foreach(println(_))


  //filter
  Array(1, 2, 3, 4, 5, 6, 7, 8, 9).filter(_ % 2 == 0).foreach(println)

  //reduce
  println((1 to 10).reduceLeft(_ * _))

  //sort
  "Mary has a little lamb".split(" ").sortWith(_.length < _.length).map(_.concat(" ")).foreach(print)
  println()

  //closure
  def mul(factor: Int) = (x: Int) => x * factor

  val triple = mul(3)
  println(s"${triple(14)}")

  //factorial using reduceLeft
  val factUsingReduce = (1 to 10).reduceLeft(_ * _)
  println(s"factorial of 10 is ${factUsingReduce}")

  //factorial using foldLeft
  val factUsingFold = (1 to 10).foldLeft(1)(_ * _)
  println(s"factorial of 10 is ${factUsingFold}")

  //calculate largest using the below function
  val larFun = (x: Int) => 10 * x - (x * x)

  def largest(f: Int => Int, s: Seq[Int]): Int ={
    s.reduceLeft((x, y) => {
      if(f(x) > f(y)) x else y
    })
  }

  println(s"largest is ${larFun(largest(larFun, 1 to 10))}")

}
