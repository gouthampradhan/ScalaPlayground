package cats.Intro
import JsonWriterInstances._
import JsonAST._
/**
  * Created by gouthamvidyapradhan on 15/01/2019
  */
object JsonInterfaceObject {
  def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = {
    writer.write(value)
  }
}
