import cas.{EquationParser, NakedEquation, Variable}
import org.scalatest.FunSpec
import workspace._

class EquationParsingTests extends FunSpec {
  describe("parsing") {
    it("works") {
      assert(EquationParser.parseEquation("E_K = 1/2 * m * v**2").contains(NakedEquation(Variable("E_K"), Variable("m") * (Variable("v") ** 2) / 2)))
    }
  }
}
