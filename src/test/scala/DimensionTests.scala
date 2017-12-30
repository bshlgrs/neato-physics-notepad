import org.scalatest.FunSpec
import workspace._

class DimensionTests extends FunSpec {
  describe("dimensions") {
    it("can combine them correctly") {
      val velocity = SiDimension(Map(Meter -> 1, Second -> -1))
      assert(velocity == Meter / Second)
      val kg_per_second = SiDimension(Map(Kilogram -> 1, Second -> -1))
      assert(velocity * kg_per_second == SiDimension(Map(Meter -> 1, Second -> -2, Kilogram -> 1)))
    }

    it("removes dimensions of power zero") {
      val velocity = SiDimension(Map(Meter -> 1, Second -> -1))
      val time = SiDimension(Map(Second -> 1))

      assert(velocity * time == SiDimension(Map(Meter -> 1)))
    }
  }

  describe("parsing") {
    it("can parse") {
      val tenMeters = PhysicalNumber(10, SiDimension(Map(Meter -> 1)), Some(Dimension.meter.toDim))
      assert(PhysicalNumber.parsePhysicalNumber("10m").toOption.contains(tenMeters))
      assert(PhysicalNumber.parsePhysicalNumber("10 m").toOption.contains(tenMeters))
      assert(PhysicalNumber.parsePhysicalNumber("10.0 m").toOption.contains(tenMeters))

      val fiveMetersPerSecond = PhysicalNumber(5, SiDimension(Map(Meter -> 1, Second -> -1)), Some(Dimension.meter.toDim / Dimension.second.toDim))
      assert(PhysicalNumber.parsePhysicalNumber("5m/s").toOption.contains(fiveMetersPerSecond))
      assert(PhysicalNumber.parsePhysicalNumber("5m s^-1").toOption.contains(fiveMetersPerSecond))
      assert(PhysicalNumber.parsePhysicalNumber("5.0 m s^-1").toOption.contains(fiveMetersPerSecond))
    }

    it("knows dimension synonyms") {
      val fourJoules = PhysicalNumber(4, SiDimension.SiJoule, Some(Dimension.joule.toDim))
      val fourJoules2 = PhysicalNumber(4, SiDimension.SiJoule, Some(Dimension.kilogram.toDim * (Dimension.meter.toDim**2) * (Dimension.second.toDim ** -2)))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 J").toOption.contains(fourJoules))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 kg m^2/s^2").toOption.contains(fourJoules2))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 kg m^2/s/s").toOption.contains(fourJoules2))

      assert(PhysicalNumber.parsePhysicalNumber("3Hz").toOption.contains(
        PhysicalNumber(3, SiDimension(Map(Second -> -1)), Some(Dimension.hertz.toDim)))
      )
    }

    it("refuses to parse broken things") {
      assert(PhysicalNumber.parsePhysicalNumber("sdf").isFailure)
      assert(PhysicalNumber.parsePhysicalNumber("5 sdf").isFailure)
      assert(PhysicalNumber.parsePhysicalNumber("5 sd+f").isFailure)
    }
  }
}
