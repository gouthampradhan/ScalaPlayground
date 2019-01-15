package cats.Intro

import cats.Intro.JsonAST._

/**
  * Created by gouthamvidyapradhan on 14/01/2019
  */
object JsonWriterInstances {

  final case class Person(name: String, email: String)

  trait JsonWriter[A] {
    def write(value: A): Json
  }

   implicit val jsonString = new JsonWriter[String] {
    override def write(value: String): Json = {
      JsString(value)
    }
  }

  implicit val jsonPerson = new JsonWriter[Person] {
    override def write(value: Person): Json = {
      JsObject(Map("name" -> JsString(value.name),
        "email" -> JsString(value.email)))
    }
  }
}
