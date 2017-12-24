package workspace

trait Dimension {
  val units: Map[SiUnit, Int]

  def *(other: Dimension): Dimension = Dimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) + other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def /(other: Dimension): Dimension = Dimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) - other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def **(other: Int): Dimension = Dimension(this.units.mapValues(_ * other))
}

case class ConcreteDimension(units: Map[SiUnit, Int]) extends Dimension

object Dimension {
  val Newton = Dimension(Map(Kilogram -> 1, Meter -> 1, Second -> -2))
  val Joule = Newton * Meter

  def apply(units: Map[SiUnit, Int]) = ConcreteDimension(units)
}

sealed trait SiUnit extends Dimension {
  val units = Map(this -> 1)
}
object Meter extends SiUnit
object Kilogram extends SiUnit
object Second extends SiUnit
object Kelvin extends SiUnit
object Ampere extends SiUnit

