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
  println(program)

  import scala.concurrent.ExecutionContext

  // We need a ContextShift[IO] before we can construct a Transactor[IO]. The passed ExecutionContext
  // is where nonblocking operations will be executed.
  implicit val cs = IO.contextShift(ExecutionContext.global)

  // A transactor that gets connections from java.sql.DriverManager and excutes blocking operations
  // on an unbounded pool of daemon threads. See the chapter on connection handling for more info.
  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", // driver classname
    "jdbc:postgresql:world", // connect URL (driver-specific)
    "postgres",              // user
    "password"                       // password
  )

  val io = program.transact(xa)
  println(io.unsafeRunSync())

  val program1: ConnectionIO[(Int, Double)] = {
    for {
      i <- sql"select 42".query[Int].unique
      j <- sql"select random()".query[Double].unique
    } yield (i, j)
  }

  val ans = program1.transact(xa).unsafeRunSync()
  println(ans)

  val program2 = {
    val v1:ConnectionIO[Int] = sql"select 42".query[Int].unique
    val v2:ConnectionIO[Double] = sql"select random()".query[Double].unique
    (v1, v2).tupled
  }

  val p2Replicated = program2.transact(xa).replicateA(5)
  p2Replicated.unsafeRunSync().foreach(println)

  sql"select name from country"
    .query[String]
    .to[List]
    .transact(xa)
    .unsafeRunSync()
    .take(5)
    .foreach(println)

  val y = xa.yolo // a stable reference is required
  import y._
  import shapeless._

  sql"select code, name, population, gnp from country"
    .query[String :: String :: Int :: Option[Double] :: HNil]
    .stream
    .take(5)
    .quick
    .unsafeRunSync


  //using case class
  case class Country(code: String, name: String, pop: Int, gnp: Option[Double])
  case class Code(code: String)

  sql"select code, name, population, gnp from country"
    .query[Country]
    .stream
    .take(5)
    .quick
    .unsafeRunSync


  case class Country1(name: String, pop: Int, gnp: Option[Double])
  case class Code1(code: String)
  //group by primary key
  sql"select code, name, population, gnp from country"
    .query[(Code1, Country1)]
    .stream
    .take(5)
    .compile.toList
    .map(x => x.toMap)
    .quick
    .unsafeRunSync

  val p = {
    sql"select code, name, population, gnp from country"
      .query[Country]
      .stream
      .transact(xa)
  }

  p.take(5).compile.toList.unsafeRunSync().foreach(println)

  def getCountries(minPop: Int): Query0[Country] = {
    sql"""select code, name, population, gnp
          from country
          where population > $minPop""".query[Country]
  }
  getCountries(5000000).quick.unsafeRunSync()

  case class Person(id: Long, name: String, age: Option[Short])

  def insert2(name: String, age: Option[Short]): ConnectionIO[Person] =
    for {
      _  <- sql"insert into person (name, age) values ($name, $age)".update.run
      id <- sql"select lastval()".query[Long].unique
      p  <- sql"select id, name, age from person where id = $id".query[Person].unique
    } yield p

  println(insert2("Goutham Vidya Pradhan", Some(36)).transact(xa).unsafeRunSync)

}

