import org.scalatest.FunSpec
import workspace._
import workspace.dimensions._

class DimensionTests extends FunSpec {
  describe("dimensions") {
    it("can combine them correctly") {
      val velocity = SiDimension.fromInts(Map(Meter -> 1, Second -> -1))
      assert(velocity == Meter / Second)
      val kg_per_second = SiDimension.fromInts(Map(Kilogram -> 1, Second -> -1))
      assert(velocity * kg_per_second == SiDimension.fromInts(Map(Meter -> 1, Second -> -2, Kilogram -> 1)))
    }

    it("removes dimensions of power zero") {
      val velocity = SiDimension.fromInts(Map(Meter -> 1, Second -> -1))
      val time = SiDimension.fromInts(Map(Second -> 1))

      assert(velocity * time == SiDimension.fromInts(Map(Meter -> 1)))
    }
  }


  describe("parsing") {
    it("can parse dimensions on their own") {
      assert(Dimension.parse("m").isSuccess)
    }

    it("can parse") {
      val tenMeters = PhysicalNumber(10, SiDimension.fromInts(Map(Meter -> 1)), Some((10, Dimension.meter.toDim)))
      assert(PhysicalNumber.parsePhysicalNumber("10m").toOption.contains(tenMeters))
      assert(PhysicalNumber.parsePhysicalNumber("10 m").toOption.contains(tenMeters))
      assert(PhysicalNumber.parsePhysicalNumber("10.0 m").toOption.contains(tenMeters))

      val fiveMetersPerSecond = PhysicalNumber(5, SiDimension.fromInts(Map(Meter -> 1, Second -> -1)), Some(5.0 -> Dimension.meter.toDim / Dimension.second.toDim))
      assert(PhysicalNumber.parsePhysicalNumber("5m/s").toOption.contains(fiveMetersPerSecond))
      assert(PhysicalNumber.parsePhysicalNumber("5m s^-1").toOption.contains(fiveMetersPerSecond))
      assert(PhysicalNumber.parsePhysicalNumber("5.0 m s^-1").toOption.contains(fiveMetersPerSecond))
    }

    it("knows dimension synonyms") {
      val fourJoules = PhysicalNumber(4, SiDimension.SiJoule, Some(4.0 -> Dimension.joule.toDim))
      val fourJoules2 = PhysicalNumber(4, SiDimension.SiJoule, Some(4.0 -> Dimension.kilogram.toDim * (Dimension.meter.toDim ** 2) * (Dimension.second.toDim ** -2)))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 J").toOption.contains(fourJoules))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 kg m^2/s^2").toOption.contains(fourJoules2))
      assert(PhysicalNumber.parsePhysicalNumber("4.0 kg m^2/s/s").toOption.contains(fourJoules2))

      assert(PhysicalNumber.parsePhysicalNumber("3Hz").toOption.contains(
        PhysicalNumber(3, SiDimension.fromInts(Map(Second -> -1)), Some(3.0 -> Dimension.hertz.toDim)))
      )
    }

    it("refuses to parse broken things") {
      assert(PhysicalNumber.parsePhysicalNumber("sdf").isFailure)
      assert(PhysicalNumber.parsePhysicalNumber("5 sdf").isFailure)
      assert(PhysicalNumber.parsePhysicalNumber("5 sd+f").isFailure)
    }

    it("gets km/hour right") {
      val n = PhysicalNumber.parsePhysicalNumber("5 km/hour").get
      println(n.toBuckTex)
    }

    it("can parse dimensionless units") {
      val nakedFour = PhysicalNumber(4, SiDimension.Dimensionless)
      assert(PhysicalNumber.parsePhysicalNumber("4.0").toOption.isDefined)
    }
  }
}
