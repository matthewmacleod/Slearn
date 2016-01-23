package slearn

import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("a string test") {
    assert("this is a string" == Stats.something)
  }

}
