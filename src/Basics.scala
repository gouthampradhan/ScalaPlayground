/**
  * Created by gouthamvidyapradhan on 24/10/2017.
  */
object Basics extends App{
    //Basics
    val one = 1.toString
    println(one)

    var greeting: String  = "Hello world"
    println(greeting)

    println("Hello".intersect("World"))

    //Control statements
    val n = 10
    println(if(n > 10) 1 else 0)

    var i = 5
    while(i >= 0){
        println(i)
        i -= 1
    }

    for(i <- 1 to 10){
        println(i)
    }

    def fac(n : Int) = {
        var fact = 1
        for(i <- 1 to 10){
            fact *= i
        }
        fact
    }

    def facRe(n : Int) : Int = {
        if(n == 0) 1
        else {
            n * facRe(n - 1)
        }
    }

    println("hellos is: " + mul("Hello"))

    def mul(s : String) : Long = {
        var p = 1
        for(i <- 0 until s.length){
            p *= s(i)
        }
        p
    }
    val nums = new Array[Int](10)

    val strs = Array("Hello", "World")

    println(strs(0))

    var value = 0
    for(i <- nums.indices){
        nums(i) = value
        value += 1
    }
    nums.foreach(println(_))
}
