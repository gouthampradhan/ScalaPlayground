package cats.Intro

import cats.Intro.JsonAST.Json
import cats.Intro.JsonWriterInstances.JsonWriter

/**
  * Created by gouthamvidyapradhan on 15/01/2019
  */
object JsonInterfaceSyntax {

  implicit class JsonSyntax[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json = {
      writer.write(value)
    }
  }
}
