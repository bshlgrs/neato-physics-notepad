import org.scalatest.FunSpec
import workspace._

class DimensionTests extends FunSpec {
  describe("dimensions") {
    it("can combine them correctly") {
      val velocity = Dimension(Map(Meter -> 1, Second -> -1))
      assert(velocity == Meter / Second)
      val kg_per_second = Dimension(Map(Kilogram -> 1, Second -> -1))
      assert(velocity * kg_per_second == Dimension(Map(Meter -> 1, Second -> -2, Kilogram -> 1)))
    }

    it("removes dimensions of power zero") {
      val velocity = Dimension(Map(Meter -> 1, Second -> -1))
      val time = Dimension(Map(Second -> 1))

      assert(velocity * time == Dimension(Map(Meter -> 1)))
    }
  }

  describe("parsing") {
    it("can parse") {
      val tenMeters = PhysicalNumber(10, Dimension(Map(Meter -> 1)))
      assert(Dimension.parsePhysicalNumber("10m").toOption.contains(tenMeters))
      assert(Dimension.parsePhysicalNumber("10 m").toOption.contains(tenMeters))
      assert(Dimension.parsePhysicalNumber("10.0 m").toOption.contains(tenMeters))

      val fiveMetersPerSecond = PhysicalNumber(5, Dimension(Map(Meter -> 1, Second -> -1)))
      assert(Dimension.parsePhysicalNumber("5m/s").toOption.contains(fiveMetersPerSecond))
      assert(Dimension.parsePhysicalNumber("5m s^-1").toOption.contains(fiveMetersPerSecond))
      assert(Dimension.parsePhysicalNumber("5.0 m s^-1").toOption.contains(fiveMetersPerSecond))
    }

    it("knows dimension synonyms") {
      val fourJoules = PhysicalNumber(4, Dimension.Joule)
      assert(Dimension.parsePhysicalNumber("4.0 J").toOption.contains(fourJoules))
      assert(Dimension.parsePhysicalNumber("4.0 kg m^2/s^2").toOption.contains(fourJoules))
      assert(Dimension.parsePhysicalNumber("4.0 kg m^2/s/s").toOption.contains(fourJoules))

      assert(Dimension.parsePhysicalNumber("3Hz").toOption.contains(PhysicalNumber(3, Dimension(Map(Second -> -1)))))
    }

    it("refuses to parse broken things") {
      assert(Dimension.parsePhysicalNumber("sdf").isFailure)
      assert(Dimension.parsePhysicalNumber("5 sdf").isFailure)
      assert(Dimension.parsePhysicalNumber("5 sd+f").isFailure)
    }
  }
}
