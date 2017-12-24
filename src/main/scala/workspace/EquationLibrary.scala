package workspace
import cas.{Expression, RationalNumber}

object EquationLibrary {
  import Dimension.{Joule,Newton}
  import cas.RationalNumber._

  val library: Map[String, Equation] = Map(
    "ke_def" -> Equation("Definition of kinetic energy",
      Expression.buildGoofily(RationalNumber(1, 2), Map("E_K" -> -1, "m" -> 1, "v" -> 2)),
      "{E_K} = \\frac12 {m} {v}^2",
      Map("E_K" -> Joule, "m" -> Kilogram, "v" -> Meter / Second),
      Map("E_K" -> "Kinetic energy", "m" -> "Mass", "v" -> "Velocity")),
    "pe_def" -> Equation("Definition of gravitational potential energy", Expression.buildGoofily(Map("E_P" -> -1, "m" -> 1, "g" -> 1, "h" -> 1)),
      "{E_P} = {m} {g} {h}",
      Map("E_P" -> Joule, "m" -> Kilogram, "g" -> Meter / Second ** 2, "h" -> Meter),
      Map("E_P" -> "Potential energy", "m" -> "Mass", "g" -> "Strength of gravity", "h" -> "Height"),
    )
  )
  // unsafe
  def getByEqId(name: String): Equation = library(name)
}
