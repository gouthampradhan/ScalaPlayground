package cats.Intro

/**
  * Created by gouthamvidyapradhan on 14/01/2019
  */
object JsonAST {
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsDouble(get: Double) extends Json
  case object JsNull extends Json
}
