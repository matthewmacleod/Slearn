package slearn

import org.scalatest.FunSuite

class StatsSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("a string test") {
    assert("this is a string" == Stats.something)
  }

  test("test the absolute value function") {
    assert(Stats.abs(-1) == 1)
  }

  test("test the factorial function") {
    val ans: BigInt = BigInt(120)
    assert(ans == Stats.factorial(BigInt(5)))
  }

  test("test l1 norm") {
    val l = ((1 to 10).toList).map(_.toDouble)
    assert( 55.0 == Stats.lpNorm(l,1.0))
  }

  test("test l2 norm") {
    val l = ((1 to 10).toList).map(_.toDouble)
    assert( 19.621416870348583 == Stats.lpNorm(l,2.0))
  }

  test("dot product 1") {
    assert(6.0 == Stats.dot(List(0.0,2.0,4.0),List(0.0,1.0,1.0)))
  }

  test(" test median odd") {
    val l = List(3.0,2.0,4.0,5.0,1.0)
    assert(3.0 == Stats.median(l))
  }

  test(" test median even") {
    val l = List(3.0,2.0,4.0,1.0)
    assert(2.5 == Stats.median(l))
  }





}
