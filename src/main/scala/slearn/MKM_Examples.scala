package slearn

/* mkm scala practice
*      and simple reference examples
*/

object MKM_Examples {


    /////////////  simple text algorithms  /////////////////

    // test
    def helloWorld(): String = {
      "Hello World!"
    }

    // Take a single-spaced <sentence>, and capitalize every <n> word starting with <offset>.
    def capitalizeEveryNthWord(sentence:String, offset:Integer, n:Integer): String = {
      val words: Array[String] = sentence.split(" ")
      val tail: Array[String] = words.drop(offset)
      var keepers: Array[String] = words.take(offset)
      var i = 0
      for (w <- tail) {
        if (i < n) {
          if (n >= 2) {
            keepers :+= w
          } else {
            keepers :+= w.capitalize
          }
        } else if (i % n == 0) {
          keepers :+= w.capitalize
        } else {
          keepers :+= w
        }
        i += 1
      }
      val capped: String = keepers.mkString(" ")
      capped
    }

    // create a string from list of strings
    def makeString(words: List[String]): String = {
      (words.head /: words.tail) (_+" "+_)
    }

    /////////////  simple maths algorithms  /////////////////

    def fibonacci(n: Int): Int = {
      @annotation.tailrec def _fib(n: Int, value: Int, acc: Int = 0): Int = n match {
        case 0 => acc
        case _ => _fib(n - 1, acc, value + acc)
      }
      _fib(n, 1)
    }

    // Determine if a number is prime
    def isPrime(n:Integer) : Boolean = {
      var answer: Boolean = false
      val ns = 2 to (n/2 + 1)
      if (n < 2) { // negative numbers and 1 are not primes
        answer = false
      } else {
        answer = ns.forall(x => n % x != 0)
      }
      answer
    }

    // Calculate the golden ratio.
    def goldenRatio(a:Double, b:Double): Double = {
      var n:Int = 50
      def _ratio(x:Double, y:Double, n:Int): Double = n match{
        case 1 => (x max y)/(x min y)
        case _ => _ratio((x max y), x+y, n-1)
      }
      _ratio(a,b,n)
    }

    // Give the square root of a number, via Newton's method:
    def squareRoot(n:Double): Double = {
      def _abs(x:Double) = if (x < 0) -1.0 * x else x
      def _square(x:Double) = x * x
      def _improve(guess:Double, x:Double) = (guess + x / guess) / 2.0
      def _converged(guess:Double, x:Double) = _abs(_square(guess)-x) < 0.00001
      def _sqrtIter(guess:Double, x:Double): Double = {
        if (_converged(guess, x)) guess
        else _sqrtIter(_improve(guess, x), x)
      }
      _sqrtIter(1.0, n)
    }


    /////////////  sorting algorithms  /////////////////

    // insert sort algorithm
    // cost ~ N^2  where N is length of list
    def insertSort(xs: List[Int]): List[Int] = xs match {
      case List() => List()
      case y::ys  => insert(y, insertSort(ys))
    }

    def insert(x: Int, xs: List[Int]): List[Int] =  xs match {
      case List() => List(x)
      case y::ys  => if (x >= y) x :: xs else y :: insert(x, ys)
    }


}
