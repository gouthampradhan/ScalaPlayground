package functional_programming

/**
  * Created by gouthamvidyapradhan on 22/12/2018
  */
object Random extends App {
  def fib(n: Int): Int = {
    if(n == 0) 0
    else if(n == 1) 1
    else {
      def go(p2: Int, p1: Int, i : Int): Int = {
        if(i == n - 1) p2 + p1
        else{
          go(p1, p1 + p2, i + 1)
        }
      }
      go(0, 1, 2)
    }
  }

  def find[A](e: Array[A], f: A => Boolean): Int ={
    def go(i: Int): Int ={
      if(i == e.length - 1) -1
      else{
        if(f(e(i))) i
        else{
          go(i + 1)
        }
      }
    }
    go(0)
  }

  def isSorted[A](a: Array[A], e: (A, A) => Boolean): Boolean = {
    if(a.length <= 1) true
    else {
      def go(p: Int, c: Int): Boolean = {
        if(c == a.length) true
        else if(!e(a(p), a(c))) false
        else{
          go(c, c + 1)
        }
      }
      go(0, 1)
    }
  }

  val findFib = 4
  println(s"The ${findFib}th Fibonacci number is ${fib(findFib)}")
}
