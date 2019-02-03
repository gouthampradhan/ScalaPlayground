package cats
import cats.instances.option._
import cats.instances.int._
import cats.syntax.semigroup._
import cats.Monoid
/**
  * Created by gouthamvidyapradhan on 21/01/2019
  */
object Monoids extends App {

  case class Order(price: Int, quantity: Int)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order]{
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order = Order(x.price + y.price, x.quantity + y.quantity)
  }

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.foldRight(Monoid[A].empty)(_ |+| _)
  }

  val orderList = List[Order](Order(0, 1), Order(1, 2))

  println(add(orderList).toString)
}
