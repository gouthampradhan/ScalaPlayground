package doobie
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._

/**
  * Created by gouthamvidyapradhan on 28/12/2018
  */
object Dao extends App {
  val program = 42.pure[ConnectionIO]
}
