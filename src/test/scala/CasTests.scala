import org.scalatest.FunSpec
import cas._

class CasTests extends FunSpec {
  val ke = Variable("ke")
  val pe = Variable("pe")
  val m = Variable("m")
  val v = Variable("v")
  val g = Variable("g")
  val h = Variable("h")

  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val zero = RationalNumber[String](0)
  val one = RationalNumber[String](1)
  val two = RationalNumber[String](2)
  val three = RationalNumber[String](3)

  describe("simplification") {
    describe("addition") {
      it("is commutative") {
        assert(x + y == y + x)
      }

      it("is associative") {
        assert((x + y) + z == x + (y + z))
      }

      it("has the identity 0") {
        assert(x == x + zero)
      }
    }

    describe("subtraction") {
      it("simplifies") {
        assert(x + y - x == y)
      }
    }

    describe("multiplication") {
      it("is commutative") {
        assert(x * y == y * x)
      }

      it("is associative") {
        assert(x * (y * z) == (x * y) * z)
      }

      it("has the identity 1") {
        assert(x * one == x)
      }

      it("has the annihilator 0") {
        assert(x * zero == zero)
      }

      it("simplifies in cases of exponents") {
        assert((x ** three) * y / (x ** two) == x * y)
        assert((x ** three) * y * x * (z ** two) / (x ** two) / z == (x ** two) * y * z)
        assert(h.sqrt * m.sqrt * g.sqrt * (m**RationalNumber(-1, 2)) * two.sqrt == g.sqrt * h.sqrt * two.sqrt)
      }

//      it("is distributive") {
//        assert((x + y) * z == x * z + y * z)
//      }
    }

    describe("division") {
      it("is the same as multiplying by the reciprocal") { assert(x / y == x * (y ** RationalNumber(-1))) }
      it("simplifies") { assert((x / y) * y == x) }
      it("simplifies numbers") { assert((x / two) * two == x) }
    }

    describe("exponentiation") {
      it("is distributive") { assert(x * x == x ** two) }
      it("is distributive with multiple variables") { assert(x * x * y * y == (x ** two) * (y ** two)) }
      it("has left annihilator 1") { assert(one ** x == one) }
      it("has right identity 1") { assert(x ** one == x) }

      it("has left annihilator 0") { assert(zero ** x == zero) }
      it("has right annihilator 0") { assert(x ** zero == one) }

      it("can simplify (x^2)^-1") { assert((x ** two) ** (zero - one) == x ** (zero - two)) }
    }

    describe("rational numbers") {
      it("simplifies addition") {
        assert(RationalNumber(2) + RationalNumber(3) == RationalNumber(5))
        assert(RationalNumber(1, 2) + RationalNumber(1, 2) == RationalNumber(1))
        assert(RationalNumber(1, 3) + RationalNumber(1, 4) == RationalNumber(7, 12))
        assert(RationalNumber(3, 4) + RationalNumber(1, 4) == RationalNumber(1))
      }

      it("simplifies subtraction") {
        assert(zero - RationalNumber(1, 2) == RationalNumber(-1, 2))
      }

      it("simplifies multiplication") {
        assert(RationalNumber(2) * RationalNumber(3) == RationalNumber(6))
        assert(RationalNumber(1, 2) * RationalNumber(1, 2) == RationalNumber(1, 4))
        assert(RationalNumber(1, 6) * RationalNumber(3, 4) == RationalNumber(1, 8))
        assert(RationalNumber(1, 6) * RationalNumber(4, 3) == RationalNumber(2, 9))
      }

      it("simplifies division") {
        assert(one / two == RationalNumber(1, 2))
        assert(RationalNumber(-1) / two == RationalNumber(-1, 2))
        assert(RationalNumber(1, 6) / RationalNumber(3, 4) == RationalNumber(2, 9))
      }

      it("exponentiates correctly") {
        assert(RationalNumber(5, 6) ** RationalNumber(1) == RationalNumber(5, 6))
        assert(RationalNumber(5, 6) ** RationalNumber(-1) == RationalNumber(6, 5))
        assert(RationalNumber(1, 6) ** RationalNumber(-1) == RationalNumber(6))
        assert(RationalNumber(3) ** RationalNumber(-1) == RationalNumber(1, 3))
        assert(RationalNumber(-3) ** RationalNumber(-1) == RationalNumber(-1, 3))

        assert(RationalNumber(5, 6) ** RationalNumber(3) == RationalNumber(125, 216))
        assert(RationalNumber(5, 6) ** RationalNumber(-3) == RationalNumber(216, 125))
      }
    }

    describe("regressions") {
      it("handles kinetic energy") {
        val half = one / two
        val keDefinition = ke - half
      }
    }
  }

  describe("solving") {
//    val keDefinition = ke - (RationalNumber(1, 2) * m * (v ** RationalNumber(2)))
//
//    it("can solve products") {
//      assert(keDefinition.solve(ke) == List(RationalNumber(1, 2) * m * (v ** RationalNumber(2))))
//      assert(keDefinition.solve(m) == List(RationalNumber(2) * ke * (v ** RationalNumber(-2))))
//      assert(keDefinition.solve(v) == List(RationalNumber(2).sqrt * ke.sqrt * (m ** RationalNumber(-1, 2))))
//    }
//
//    it("can solve sums") {
//      assert((ke + pe).solve(ke) == List(pe * RationalNumber(-1)))
//      assert((ke - pe).solve(ke) == List(pe))
//    }
  }

  describe("vars") {
    val x = Variable("x")
    val y = Variable("y")

    it("knows about vars") {
      assert((x + y).vars == Set("x", "y"))
      assert((x * y).vars == Set("x", "y"))
      assert((x ** y).vars == Set("x", "y"))
      assert((x ** y + RationalNumber(4, 3)).vars == Set("x", "y"))
      assert((x * y + RationalNumber(3, 1)).vars == Set("x", "y"))
      assert((x * RationalNumber(3, 1)).vars == Set("x"))
      assert(RationalNumber(3, 1).vars == Set())
    }
  }
}
