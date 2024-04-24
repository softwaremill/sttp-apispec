package sttp.apispec.validation

import org.scalatest.funsuite.AnyFunSuite

class BoundsTest extends AnyFunSuite {
  test("unbounded") {
    val b = Bounds[Int](None, None)
    assert(b.toString == "(-inf,inf)")
    assert(b.contains(b))
    assert(b.contains(Bounds(None, Some(Bound.inclusive(1)))))
    assert(b.contains(Bounds(None, Some(Bound.exclusive(1)))))
    assert(b.contains(Bounds(Some(Bound.inclusive(1)), None)))
    assert(b.contains(Bounds(Some(Bound.exclusive(1)), None)))
    assert(b.contains(Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(2)))))
  }

  test("right bounded") {
    val b = Bounds(None, Some(Bound.inclusive(10)))
    assert(b.toString == "(-inf,10]")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(b.contains(Bounds(None, Some(Bound.exclusive(10)))))
    assert(b.contains(Bounds(None, Some(Bound.inclusive(9)))))
    assert(!b.contains(Bounds(None, Some(Bound.inclusive(11)))))
    assert(b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(11)))))
  }

  test("right bounded open") {
    val b = Bounds(None, Some(Bound.exclusive(10)))
    assert(b.toString == "(-inf,10)")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(b.contains(Bounds(None, Some(Bound.exclusive(9)))))
    assert(b.contains(Bounds(None, Some(Bound.inclusive(9)))))
    assert(!b.contains(Bounds(None, Some(Bound.inclusive(10)))))
    assert(!b.contains(Bounds(None, Some(Bound.inclusive(11)))))
    assert(b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.exclusive(10)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(11)))))
  }

  test("left bounded") {
    val b = Bounds(Some(Bound.inclusive(10)), None)
    assert(b.toString == "[10,inf)")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(b.contains(Bounds(Some(Bound.exclusive(10)), None)))
    assert(!b.contains(Bounds(Some(Bound.inclusive(9)), None)))
    assert(b.contains(Bounds(Some(Bound.inclusive(11)), None)))
    assert(b.contains(Bounds(Some(Bound.inclusive(10)), Some(Bound.inclusive(15)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(9)), Some(Bound.inclusive(15)))))
  }

  test("left bounded open") {
    val b = Bounds(Some(Bound.exclusive(10)), None)
    assert(b.toString == "(10,inf)")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(b.contains(Bounds(Some(Bound.inclusive(11)), None)))
    assert(b.contains(Bounds(Some(Bound.exclusive(10)), None)))
    assert(!b.contains(Bounds(Some(Bound.inclusive(10)), None)))
    assert(!b.contains(Bounds(Some(Bound.inclusive(9)), None)))
    assert(!b.contains(Bounds(Some(Bound.inclusive(10)), Some(Bound.inclusive(15)))))
    assert(b.contains(Bounds(Some(Bound.exclusive(10)), Some(Bound.inclusive(15)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(9)), Some(Bound.inclusive(15)))))
  }

  test("bounded") {
    val b = Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))
    assert(b.toString == "[0,10]")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(b.contains(Bounds(Some(Bound.exclusive(0)), Some(Bound.inclusive(10)))))
    assert(b.contains(Bounds(Some(Bound.exclusive(0)), Some(Bound.exclusive(10)))))
    assert(b.contains(Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(9)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(11)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(-1)), Some(Bound.inclusive(9)))))
  }

  test("bounded open") {
    val b = Bounds(Some(Bound.exclusive(0)), Some(Bound.exclusive(10)))
    assert(b.toString == "(0,10)")
    assert(b.contains(b))
    assert(!b.contains(Bounds(None, None)))
    assert(!b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.inclusive(10)))))
    assert(!b.contains(Bounds(Some(Bound.exclusive(0)), Some(Bound.inclusive(10)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(0)), Some(Bound.exclusive(10)))))
    assert(b.contains(Bounds(Some(Bound.exclusive(0)), Some(Bound.exclusive(10)))))
    assert(b.contains(Bounds(Some(Bound.exclusive(1)), Some(Bound.exclusive(9)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(1)), Some(Bound.inclusive(11)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(-1)), Some(Bound.inclusive(9)))))
    assert(!b.contains(Bounds(Some(Bound.inclusive(-1)), Some(Bound.inclusive(11)))))
    assert(!b.contains(Bounds(Some(Bound.exclusive(-1)), Some(Bound.exclusive(11)))))
  }
}


