import cas.{EquationParser, Variable}
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
  }
}
