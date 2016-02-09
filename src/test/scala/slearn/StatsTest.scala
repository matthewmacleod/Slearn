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




}
