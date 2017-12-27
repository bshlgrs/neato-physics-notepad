import org.scalatest.FunSpec
import workspace._

class NumberParserTests extends FunSpec {
  describe("parsing") {
    it("can parse") {
      val tenMeters = PhysicalNumber(10, Dimension(Map(Meter -> 1)))
      assert(NumberParser.parse("10m").toOption.contains(tenMeters))
      assert(NumberParser.parse("10 m").toOption.contains(tenMeters))
      assert(NumberParser.parse("10.0 m").toOption.contains(tenMeters))

      val fiveMetersPerSecond = PhysicalNumber(5, Dimension(Map(Meter -> 1, Second -> -1)))
      assert(NumberParser.parse("5m/s").toOption.contains(fiveMetersPerSecond))
      assert(NumberParser.parse("5m s^-1").toOption.contains(fiveMetersPerSecond))
      assert(NumberParser.parse("5.0 m s^-1").toOption.contains(fiveMetersPerSecond))
    }

    it("knows dimension synonyms") {
      val fourJoules = PhysicalNumber(4, Dimension.Joule)
      assert(NumberParser.parse("4.0 J").toOption.contains(fourJoules))
      assert(NumberParser.parse("4.0 kg m^2/s^2").toOption.contains(fourJoules))
      assert(NumberParser.parse("4.0 kg m^2/s/s").toOption.contains(fourJoules))

      assert(NumberParser.parse("3Hz").toOption.contains(PhysicalNumber(3, Dimension(Map(Second -> -1)))))
    }

    it("refuses to parse broken things") {
      assert(NumberParser.parse("sdf").isFailure)
      assert(NumberParser.parse("5 sdf").isFailure)
      assert(NumberParser.parse("5 sd+f").isFailure)
    }
  }
}

