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
    // use a foldleft /: which creates left-leaning operation tree
    def makeString(words: List[String]): String = {
      (words.head /: words.tail) (_ + " " + _)
    }

    // xs is the accumulator, and since going L to R,
    // starting with empty list, the original list will be reversed
    def rev[T](list: List[T]): List[T] = {
      list.foldLeft(List[T]()) ((xs, x) => x::xs)
    }

    def reverseLeft[T](xs: List[T]): List[T] = {
      (List[T]() /: xs) {(ys,y) => y::ys}
    }


    /////////////  simple maths algorithms  /////////////////

    def sum(a: Int, b: Int): Int = {
      a + b
    }

    def product(a: Int, b:Int): Int = {
      a * b
    }

    def sum(ns: List[Int]): Int = (0 /: ns) (_+_)

    def product(ns: List[Int]): Int = (1 /: ns) (_*_)

    // return power do x^y
    def power(x: Double, y: Int): Double = {
      @annotation.tailrec def _pow(x:Double, n:Int, acc:Double=1): Double = n match {
        case 0 => acc
        case _ => _pow(x,n-1,acc*x)
      }
      _pow(x,y)
    }

    def quotient(numerator: Int, denominator: Int): Int =  {
      numerator/denominator
    }

    def quotient(numerator: Double, denominator: Double): Int = {
      (numerator/denominator).floor.toInt
    }

    def remainder(numerator: Int, denominator: Int): Int =  {
      numerator % denominator
    }

    def remainder(numerator: Double, denominator: Double): Int =  {
      (numerator % denominator).toInt
    }

    def square(n: Int): Int = n*n

    // def squareAll(myList:List[Int]): List[Int] = myList.map((x:Int) => square(x))
    def squareAll(myList: List[Int]): List[Int] = myList.map(square(_))

    def sumSquareAll(myList: List[Int]): Int = squareAll(myList).reduce(_+_)

    def productAll(myList: List[Int]): Int = myList.reduce(_*_)

    def productAll2(myListA: List[Int], myListB: List[Int]): List[Int] = {
      for ((a,b) <- myListA zip myListB) yield a*b
    }

    def dotProduct(listA: List[Int], listB: List[Int]): Int = {
      val productVector: List[Int] = for ((a,b) <- listA zip listB) yield a*b
      productVector.reduce(_+_)
    }

    def nthPrime(s: Stream[Int], n: Int): Int = {
      (s.filter(isPrime(_)))(n)
    }

    def takeNPrimes(s: Stream[Int], n: Int): List[Int] = {
      (s.filter(isPrime(_))).take(n).toList
    }

    def fibonacci(n: Int): Int = {
      @annotation.tailrec def _fib(n: Int, value: Int, acc: Int = 0): Int = n match {
        case 0 => acc
        case _ => _fib(n - 1, acc, value + acc)
      }
      _fib(n, 1)
    }

    // Determine if a number is prime
    def isPrime(n: Int): Boolean = {
      val ns = 2 to n-1
      n match {
        case i if i < 2 => return false
        case _ => { val divisorFound: Boolean = ns.exists(x => n % x == 0)
                    if (divisorFound) return false else return true }
      }
    }

    // Calculate the golden ratio.
    def goldenRatio(a: Double, b: Double): Double = {
      var n:Int = 50
      def _ratio(x:Double, y:Double, n:Int): Double = n match{
        case 1 => (x max y)/(x min y)
        case _ => _ratio((x max y), x+y, n-1)
      }
      _ratio(a,b,n)
    }

    // Give the square root of a number, via Newton's method:
    def squareRoot(n: Double): Double = {
      def _abs(x: Double) = if (x < 0) -1.0 * x else x
      def _square(x: Double) = x * x
      def _improve(guess: Double, x: Double) = (guess + x / guess) / 2.0
      def _converged(guess: Double, x: Double) = _abs(_square(guess)-x) < 0.00001
      def _sqrtIter(guess: Double, x: Double): Double = {
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
