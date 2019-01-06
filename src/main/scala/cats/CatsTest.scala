package cats

object CatsTest extends App {

  import cats.implicits._
  val aMap = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combineMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
  println(combineMap.get("foo"))

  println(Monoid[String].empty)
  println(Monoid[String].combineAll(List("a", "b", "c")))
  println(Monoid[String].combineAll(List()))

  import Helper._
  5.times(println("Hi"))

}
