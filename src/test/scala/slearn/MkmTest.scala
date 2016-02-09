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

  test("make string example"){
    assert("an easy test" == MKM_Examples.makeString(List("an", "easy", "test")))
  }



}
