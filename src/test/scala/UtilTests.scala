import cas.{EquationParser, Variable}
import org.scalatest.FunSpec
import workspace._

class UtilTests extends FunSpec {
  describe("printing numbers") {
    it("works") {
      assert(Util.showNumber(0) == "0")
      assert(Util.showNumber(3.2) == "3.2")
      assert(Util.showNumber(1.2345678) == "1.235")
      assert(Util.showNumber(1.00003) == "1.000")
      assert(Util.showNumber(2.0000000003) == "2")
      assert(Util.showNumber(12) == "12")
      assert(Util.showNumber(12345) == "1.235e5")
      assert(Util.showNumber(120 * 1000 * 1000) == "1.2e8")
      assert(Util.showNumber(1.234567e12) == "1.235e12")
      assert(Util.showNumber(-1.23e12) == "-1.23e12")
      assert(Util.showNumber(-1e-12) == "-1e-12")
      assert(Util.showNumber(-1.23456e-12) == "-1.235e-12")

      /*

     Rules:

     - If the shorter version of the number is just as accurate, use it
     - ???
       */
    }
  }
}
