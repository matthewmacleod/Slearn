package slearn

object Stats {
    val something = "this is a string"

    def abs(n: Int): Int =
      if (n < 0) -n
      else n


    def mean(xs: List[Int]): Double = xs match {
      case Nil => 0.0
      case ys => ys.reduceLeft(_ + _) / ys.size.toDouble
    }


    def std(xs: List[Int], avg: Double): Double = xs match {
      case Nil => 0.0
      case ys => math.sqrt((0.0 /: ys) {
        (a,e) => a + math.pow(e - avg, 2.0)
      } / (xs.size-1))
    }

    def factorial(n: Int): Int = {
      def go(n: Int, acc: Int): Int =
        if (n <= 0) acc
        else go(n-1, n*acc)

      go(n, 1)
    }


    def fact(n: Int) = 1 to n reduceLeft(_*_)


    def main(args: Array[String]) {
          println("Running the Stats application.")
    }
}
