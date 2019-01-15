package cats.Intro
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
}
