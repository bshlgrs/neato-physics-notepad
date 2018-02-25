import org.scalatest.FunSpec
import cas._
import workspace._
import workspace.dimensions.{ConcreteDimensionInference, Kilogram, Meter}

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
        assert((x + y).equivalent(y + x))
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
        assert((x * y).equivalent(y * x))
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
        assert((h.sqrt * m.sqrt * g.sqrt * (m ** RationalNumber[String](-1, 2)) * two.sqrt).equivalent(g.sqrt * h.sqrt * two.sqrt))
      }

//      it("is distributive") {
//        assert((x + y) * z == x * z + y * z)
//      }
    }

    describe("division") {
      it("is the same as multiplying by the reciprocal") { assert(x / y == x * (y ** -1)) }
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
      def RationalNumberS(n: Int, d: Int = 1) = RationalNumber[String](n, d)
      it("simplifies addition") {
        assert(RationalNumberS(2) + RationalNumberS(3) == RationalNumberS(5))
        assert(RationalNumberS(1, 2) + RationalNumberS(1, 2) == RationalNumberS(1))
        assert(RationalNumberS(1, 3) + RationalNumberS(1, 4) == RationalNumberS(7, 12))
        assert(RationalNumberS(3, 4) + RationalNumberS(1, 4) == RationalNumberS(1))
      }

      it("simplifies subtraction") {
        assert(zero - RationalNumberS(1, 2) == RationalNumberS(-1, 2))
      }

      it("simplifies multiplication") {
        assert(RationalNumberS(2) * RationalNumberS(3) == RationalNumberS(6))
        assert(RationalNumberS(1, 2) * RationalNumberS(1, 2) == RationalNumberS(1, 4))
        assert(RationalNumberS(1, 6) * RationalNumberS(3, 4) == RationalNumberS(1, 8))
        assert(RationalNumberS(1, 6) * RationalNumberS(4, 3) == RationalNumberS(2, 9))
      }

      it("simplifies division") {
        assert(one / two == RationalNumberS(1, 2))
        assert(RationalNumberS(-1) / two == RationalNumberS(-1, 2))
        assert(RationalNumberS(1, 6) / RationalNumberS(3, 4) == RationalNumberS(2, 9))
      }

      it("exponentiates correctly") {
        assert(RationalNumberS(5, 6) ** RationalNumberS(1) == RationalNumberS(5, 6))
        assert(RationalNumberS(5, 6) ** RationalNumberS(-1) == RationalNumberS(6, 5))
        assert(RationalNumberS(1, 6) ** RationalNumberS(-1) == RationalNumberS(6))
        assert(RationalNumberS(3) ** RationalNumberS(-1) == RationalNumberS(1, 3))
        assert(RationalNumberS(-3) ** RationalNumberS(-1) == RationalNumberS(-1, 3))

        assert(RationalNumberS(5, 6) ** RationalNumberS(3) == RationalNumberS(125, 216))
        assert(RationalNumberS(5, 6) ** RationalNumberS(-3) == RationalNumberS(216, 125))
      }
    }

    describe("regressions") {
      it("handles kinetic energy") {
        val half = one / two
        val keDefinition = ke - half
      }

      it("handles substitution correctly") {
        assert((x + y + z).substituteMany(Set("x", "y", "z"), "y") == y * 3)
      }

      it("multiplies rational square roots correctly") {
        assert((one / two).sqrt * two.sqrt == RationalNumber(1))
        assert((one / two).sqrt * three.sqrt == RationalNumber(3, 2).sqrt)
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
    it("can do a thing that looks like Lorentz boost") {
      val expr = EquationParser.parseExpression("γ - 1/(1 - v/c)").get.solve("v").head
      println(expr)
    }

    it("can do a relatively easy one") {
      val expr = EquationParser.parseExpression("γ - 1/(v/c)").get.solve("v").head
      assert(expr == Variable("c") / Variable("γ"))
    }

    it("can do quadratic equations") {
      val solutions = EquationParser.parseExpression("y - 1 + 2*x + x**2").get.solve("x")
      println(solutions)
      assert(solutions.size == 2)
    }
  }

  describe("vars") {

    it("knows about vars") {
      assert((x + y).vars == Set("x", "y"))
      assert((x * y).vars == Set("x", "y"))
      assert((x ** y).vars == Set("x", "y"))
      assert((x ** y + RationalNumber[String](4, 3)).vars == Set("x", "y"))
      assert((x * y + RationalNumber[String](3, 1)).vars == Set("x", "y"))
      assert((x * RationalNumber[String](3, 1)).vars == Set("x"))
      assert(RationalNumber(3, 1).vars == Set())
    }
  }

  describe("display stuff") {
    it("knows how to order stuff") {
      assert(ExpressionDisplay.orderWithConstantsFirst(List[Expression[String]](two, x)) == List(two, x))
    }

    it("can render stuff") {
//      println(CompileToBuckTex.compileExpression((x * y).mapVariables(name => Text(name))))
//      println(CompileToBuckTex.compileExpression((x / y).mapVariables(name => Text(name))))
      println(CompileToBuckTex.compileExpression((x.sqrt * z.sqrt / y * RationalNumber[String](2).sqrt).mapVariables(name => Text(name))))
//      println(CompileToBuckTex.compileExpression((x * 2).mapVariables(name => Text(name))))

    }
  }

  describe("differentiation") {
    it("works") {
      assert((x + y).differentiate("x") == one)

      assert((x * y).differentiate("x") == y)
    }
  }


  describe("guessing floating point numbers") {
    it ("can guess") {
      assert(RationalNumber.makeFromDouble(4.0).contains(RationalNumber(4)))
      assert(RationalNumber.makeFromDouble(5.5).contains(RationalNumber(11, 2)))
      assert(RationalNumber.makeFromDouble(5.2).contains(RationalNumber(26, 5)))
      assert(RationalNumber.makeFromDouble(1.132439502).isEmpty)
    }
  }

  describe("getting types") {
    it("handles NamedNumbers correctly") {
      println(PhysicalConstant.G.namedNumber.dimension)
      println((PhysicalConstant.G.namedNumber * Variable("m1") * Variable("m2") / (Variable("r") ** 2)).calculateDimension({
        case "m1" => ConcreteDimensionInference(Kilogram)
        case "m2" => ConcreteDimensionInference(Kilogram)
        case "r" => ConcreteDimensionInference(Meter)
      }))
    }
  }
}
