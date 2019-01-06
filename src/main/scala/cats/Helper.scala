package cats

object Helper {

  implicit class IntWithTimes(x: Int){
    def times[A](f: => A): Unit = {
      def loop(i: Int): Unit = {
        if(i < x){
          f
        } else {
          loop(i - 1)
        }
      }
      loop(x)
    }
  }
}
