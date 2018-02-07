import cas.{EquationParser, RationalNumber, RealNumber, Variable}
import org.scalatest.FunSpec
import workspace._

class EquationParsingTests extends FunSpec {
  describe("parsing") {
    it("works") {
      assert(EquationParser.parseEquation("E_K = 1/2 * m * v**2").contains(CustomEquation(Variable("E_K"), Variable("m") * (Variable("v") ** 2) / 2)))
    }

    it("can do functions") {
      println(EquationParser.parseEquation("x = sin(y)"))
    }

    it("test") {
      println(EquationParser.parseEquation("E = E_"))
    }

    it("handles sqrt") {
      assert(EquationParser.parseEquation("l = sqrt(A)").contains(CustomEquation(Variable("l"), Variable("A").sqrt)))
    }

    it("handles floating point numbers") {
      assert(EquationParser.number.parse("1.2").get.value match {
        case RealNumber(value) => math.abs(value - 1.2) < 0.001
        case _ => false
      })
      assert(EquationParser.parseEquation("l = 1.5").contains(CustomEquation(Variable("l"), RealNumber(1.5))))
    }

    it("allows carat for exponentiation") {
      assert(EquationParser.parseEquation("l = x^2").contains(CustomEquation(Variable("l"), Variable("x") ** 2)))
    }

    it("can handle parentheses") {
//      print(EquationParser.expr.parse("(v+2)"))
      assert(EquationParser.parseEquation("y = (v+2)").contains(CustomEquation(Variable[String]("y"), Variable[String]("v") + RationalNumber[String](2))))
    }
  }
}
