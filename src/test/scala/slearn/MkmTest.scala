package slearn

import org.scalatest.FunSuite

class MkmSuite extends FunSuite {

  // test
  test("nazdar") {
    assert("Hello World!" == MKM_Examples.helloWorld)
  }

  test(" test caps") {
    val sentence: String = "Lorem ipsum dolor sit amet"
    assert("Lorem ipsum Dolor sit Amet" == MKM_Examples.capitalizeEveryNthWord(sentence, 0, 2))
  }

  test("x test caps") {
    val sentence: String = "Lorem ipsum dolor sit amet"
    assert("Lorem ipsum Dolor Sit Amet" == MKM_Examples.capitalizeEveryNthWord(sentence, 2, 1))
  }

  test("fibonacci 0") {
    assert(0 == MKM_Examples.fibonacci(0))
  }

  test("fibonacci 1") {
    assert(1 == MKM_Examples.fibonacci(1))
  }

  test("fibonacci 2") {
    assert(1 == MKM_Examples.fibonacci(2))
  }

  test("fibonacci 3") {
    assert(2 == MKM_Examples.fibonacci(3))
  }

  test("fibonacci 20") {
    assert(6765 == MKM_Examples.fibonacci(20))
  }

  test("golden ratio"){
    assert(1.618033988749895 == MKM_Examples.goldenRatio(5.0, 8.0))
  }

  test("sqrt 25"){
    assert(5.000000000053722 == MKM_Examples.squareRoot(25.0))
  }

  test("sqrt 2"){
    assert(1.4142156862745097 == MKM_Examples.squareRoot(2.0))
  }

  test("sqrtStream 2"){
    assert(1.4142156862745097 == MKM_Examples.sqrtStream(2.0))
  }

  test("make string example"){
    assert("an easy test" == MKM_Examples.makeString(List("an", "easy", "test")))
  }

  test("simple list reverse"){
    assert(List(3,2,1) == MKM_Examples.reverseLeft(List(1,2,3)))
  }

  test("simple sum"){
    assert(7 == MKM_Examples.sum(3,4))
  }

  test("simple sum list"){
    assert(8 == MKM_Examples.sum(List(3,4,1)))
  }

  test("simple product"){
    assert(12 == MKM_Examples.product(3,4))
  }

  test("simple product list"){
    assert(24 == MKM_Examples.product(List(3,4,2)))
  }

  test("power test 2"){
    assert(9.0 == MKM_Examples.power(3.0,2))
  }

  test("power test 3"){
    assert(27.0 == MKM_Examples.power(3.0,3))
  }

  test("power test 4"){
    assert(81.0 == MKM_Examples.power(3.0,4))
  }

  test("q1"){
    assert(2 == MKM_Examples.quotient(5,2))
  }

  test("q2"){
    assert(2 == MKM_Examples.quotient(5.0,2.0))
  }

  test("r1"){
    assert(1 == MKM_Examples.remainder(5,2))
  }

  test("r1b"){
    assert(1 == MKM_Examples.remainder(5.0,2.0))
  }

  test("squareAll test"){
    assert(List(1,4,9) == MKM_Examples.squareAll(List(1,2,3)))
  }

  test("sumsquareAll test"){
    assert(14 == MKM_Examples.sumSquareAll(List(1,2,3)))
  }

  test("productall test"){
    assert(24 == MKM_Examples.productAll(List(1,2,3,4)))
  }

  test("productall2 test"){
    assert(List(10,200,3000) == MKM_Examples.productAll2(List(1,2,3),List(10,100,1000)))
  }

  test("dot product 1"){ assert(6 == MKM_Examples.dotProduct(List(0,2,4),List(0,1,1))) }

  test("test isp prime3"){ assert( true == MKM_Examples.isPrime(3)) }

  test("test isp prime4"){ assert( false == MKM_Examples.isPrime(4)) }

  test("stream test 1"){ assert(7 == MKM_Examples.nthPrime((1 to 100).toStream,3)) }

  test("stream test 2"){ assert(List(2, 3, 5) == MKM_Examples.takeNPrimes((1 to 100).toStream,3)) }

  test("test primes"){ assert(List(2, 3, 5) == MKM_Examples.primes(3)) }

}
