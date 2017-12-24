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

  it("can do parsing") {
    assert(Dimension.parse("m s^2") == Dimension(Map(Meter -> 1, Second -> 2)))
    assert(Dimension.parse("kg s^-2") == Dimension(Map(Kilogram -> 1, Second -> -2)))
    assert(Dimension.parse("J") == Dimension.Joule)
  }
}
