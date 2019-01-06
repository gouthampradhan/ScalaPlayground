package functional_programming
/**
  * Created by gouthamvidyapradhan on 22/12/2018
  */
object Chapter1 extends App {

  trait Monoid[A] {
    def mappend(a1: A, a2: A): A
    def mzero: A
  }

  object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      override def mappend(a: Int, b: Int): Int = a + b
      override def mzero: Int = 0
    }

    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
      override def mappend(a: String, b: String): String = a + b
      override def mzero: String = ""
    }
  }

  def sum[A: Monoid](xs: scala.collection.immutable.List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mappend)
  }
}

