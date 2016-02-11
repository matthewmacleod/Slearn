package slearn

object Stats {
    val something = "this is a string"

    def abs(n: Double): Double = if (n < 0) -1 * n else n

    def mean(xs: List[Double]): Double = xs match {
      case Nil => 0.0
      case ys => ys.reduceLeft(_ + _) / ys.size.toDouble
    }

    // return median
    def median(xs: List[Double]): Double = {
      val sList = xs.sortWith(_>_)
      val n = sList.size
      val half = n/2
      n % 2 == 0 match {
        case false => return { sList(half) }
        case true => return { (sList(half)+sList(half-1))/2.0 }
      }
    }


    def std(xs: List[Double], avg: Double): Double = xs match {
      case Nil => 0.0
      case ys => math.sqrt((0.0 /: ys) {
        (a,e) => a + math.pow(e - avg, 2.0)
      } / (xs.size-1))
    }

    def lpNorm(xs: List[Double], p:Double): Double = {
      val summed = xs.foldLeft(0.0) ((acc, x) => acc + math.pow(abs(x),p))
      math.pow(summed, 1.0/p)
    }

    def dot(listA: List[Double], listB: List[Double]): Double = {
      val sums = for ( (a, b) <- listA zip listB) yield a * b
      sums.reduce(_+_)
    }

    @annotation.tailrec
    def factorial(n: BigInt, acc: BigInt = BigInt(1)): BigInt = {
      if (n <= 0) acc
      else factorial(n-1, n*acc)
    }

    def fact(n: Int) = 1 to n reduceLeft(_*_)

    //// main
    def main(args: Array[String]) {
          println("Running the Stats application.")
    }
}
