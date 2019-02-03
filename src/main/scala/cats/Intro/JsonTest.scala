package cats.Intro
import java.util.Date

import JsonWriterInstances._
import JsonInterfaceSyntax._

/**
  * Created by gouthamvidyapradhan on 15/01/2019
  */
object JsonTest extends App {

  val json = JsonInterfaceObject.toJson("Jason Bourne")
  println(json)
  println("Jason Bourne".toJson)

  println(Person("Jason Bourne", "jsonBourne@virtualworld.com").toJson)

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._
  //val eqInt = Eq[Int]

  //eqInt.eqv(12, 12)
  //implicit val intEq: Eq[Int] = Eq.instance[Int]((x, y) => x == y)

  println(12 === 12)

  //println((Some(1): Option[Int]) === (None: Option[Int]))

  implicit val dateEq = Eq.instance[Date]((x, y) => x.equals(y))
  val date1 = new Date
  val date2 = new Date
  println(date1 === date2)

}
