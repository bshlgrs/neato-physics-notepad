import org.scalatest.FunSpec
import cas._

class CasTests extends FunSpec {
  describe("simplification") {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val zero = RationalNumber[String](0)
    val one = RationalNumber[String](1)
    val two = RationalNumber[String](2)

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

//      it("is distributive") {
//        assert((x + y) * z == x * z + y * z)
//      }
    }

    describe("exponentiation") {
      it("is distributive") { assert(x * x == x ** two) }
      it("is distributive with multiple variables") { assert(x * x * y * y == (x ** two) * (y ** two)) }
    it("has left annihilator 1") { assert(one ** x == one) }
      it("has right identity 1") { assert(x ** one == x) }

      it("has left annihilator 0") { assert(zero ** x == zero) }
      it("has right annihilator 0") { assert(x ** zero == one) }
    }

    describe("rational numbers") {
      it("simplifies addition") {
        assert(RationalNumber(2) + RationalNumber(3) == RationalNumber(5))
        assert(RationalNumber(1, 2) + RationalNumber(1, 2) == RationalNumber(1))
        assert(RationalNumber(1, 3) + RationalNumber(1, 4) == RationalNumber(7, 12))
        assert(RationalNumber(3, 4) + RationalNumber(1, 4) == RationalNumber(1))
      }

      it("simplifies multiplication") {
        assert(RationalNumber(2) * RationalNumber(3) == RationalNumber(6))
        assert(RationalNumber(1, 2) * RationalNumber(1, 2) == RationalNumber(1, 4))
        assert(RationalNumber(1, 6) * RationalNumber(3, 4) == RationalNumber(1, 8))
      }
    }
  }
}
