package functional_programming.Chapter7

import functional_programming.{List, Nil, None, Option, Some}

trait Monoid[A] {
  def op(op1: A, op2: A): A
  def zero: A
}

object Chapter7 extends App {

  val stringMonoid = new Monoid[String] {
    override def op(op1: String, op2: String): String = {
      op1 + op2
    }
    override def zero: String = {
      ""
    }
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(op1: List[A], op2: List[A]): List[A] = op1 ++ op2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(op1: Int, op2: Int): Int = op1 + op2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(op1: Int, op2: Int): Int = op1 * op2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(op1: Boolean, op2: Boolean): Boolean = op1 || op2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(op1: Boolean, op2: Boolean): Boolean = op1 && op2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(op1: Option[A], op2: Option[A]): Option[A] = {
     op1 orElse op2
    }
    override def zero: Option[A] = None
  }

  def intCompare: Monoid[Int] = new Monoid[Int] {
    override def op(op1: Int, op2: Int): Int = op1.compareTo(op2)
    override def zero: Int = 0
  }

  def endoFunction[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(op1: A => A, op2: A => A): A => A = {
      a: A => op1(op2(a))
    }
    override def zero: A => A = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldRight(m.zero)(m.op)
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else if(v.length == 1) f(v(0))
    else {
      val tuple = v.splitAt(v.length / 2)
      m.op(tuple._1.map(f).foldLeft(m.zero)(m.op), tuple._2.map(f).foldLeft(m.zero)(m.op))
    }
  }

  def isOrdered(xs: IndexedSeq[Int]): Boolean = {

    val ascMonoid: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(op1: Option[(Int, Int, Boolean)], op2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        op1 match {
          case None => op2
          case Some(x) => op2 match {
            case None => op1
            case Some(y) => {
              if(!x._3 || !y._3) {
                Some((-1, -1, false))
              }
              else{
                if(x._2 <= y._1){
                  Some(x._1, y._2, true)
                } else{
                  Some((-1, -1, false))
                }
              }
            }
          }
        }
      }

      override def zero: Option[(Int, Int, Boolean)] = None
    }
    val result = xs.map(x => Some(x, x, true)).foldLeft(ascMonoid.zero)(ascMonoid.op)
    result.getOrElse((-1, -1, false))._3
  }

  println("ordered result is: " + isOrdered(IndexedSeq(1, 2, 5, 5, 55, 667)))
}
