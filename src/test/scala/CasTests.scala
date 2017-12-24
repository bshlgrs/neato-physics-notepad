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
      it("has left annihilator 1") { assert(one ** x == one) }
      it("has right identity 1") { assert(x ** one == x) }

      it("has left annihilator 0") { assert(zero ** x == zero) }
      it("has right annihilator 0") { assert(x ** zero == one) }
    }

  }
}
