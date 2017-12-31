package workspace
import cas.{Expression, RationalNumber}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Gem.EquationLibrary")
object EquationLibrary {
  import SiDimension._

  val library: Map[String, LibraryEquation] = Map(
    "ke_def" -> LibraryEquation("Definition of kinetic energy",
      Expression.buildGoofily(RationalNumber(1, 2), Map("E_K" -> -1, "m" -> 1, "v" -> 2)),
      (f: String => BuckTex) => CompileToBuckTex.centeredBox(List(f("E_K"), Text(" = ½"), f("m"), f("v"), Sup(List(Text("2"))))),
      Map("E_K" -> SiJoule, "m" -> Kilogram, "v" -> Meter / Second),
      Map("E_K" -> "Kinetic energy", "m" -> "Mass", "v" -> "Velocity"),
      Set("kinetic", "energy")),
    "pe_def" -> LibraryEquation("Definition of gravitational potential energy", Expression.buildGoofily(Map("E_P" -> -1, "m" -> 1, "g" -> 1, "h" -> 1)),
      (f: String => BuckTex) => CompileToBuckTex.centeredBox(List(f("E_P"), Text(" = "), f("m"), f("g"), f("h"))),
      Map("E_P" -> SiJoule, "m" -> Kilogram, "g" -> Meter / (Second ** 2), "h" -> Meter),
      Map("E_P" -> "Potential energy", "m" -> "Mass", "g" -> "Strength of gravity", "h" -> "Height"),
      Set("gravitational", "potential", "energy")
    ),
    "hookes_law" -> Equation.buildQuickly("Hooke's law", ("F", "Spring force", SiNewton),
      Map("k" -> (1, "Spring constant", SiNewton / Meter),
        "x" -> (1, "Displacement", Meter)),
      "spring hooke's law",
      RationalNumber(-1)
    ),
    "energy_of_spring" -> Equation.buildQuickly("Energy of spring", ("E_S", "Spring potential energy", SiJoule),
      Map("k" -> (1, "Spring constant", SiNewton / Meter),
        "x" -> (2, "Displacement", Meter)),
      "spring hooke's law",
      RationalNumber(1, 2)
    ),
    "force_due_to_gravity" -> Equation.buildQuickly("Force due to gravity", ("F", "Gravitational force", SiNewton),
      Map("g" -> (1, "Gravitational acceleration", Meter / Second / Second),
        "m" -> (1, "Mass", Kilogram)),
      "force gravity"
    ),
//    'momentum_def' => Equation.buildFaster("Definition of momentumj")
    "ohms_law" -> Equation.buildFaster(
      "Ohm's law",
      "I = V / R",
      Map("I" -> ("Current", Ampere), "V" -> ("Voltage", SiVolt), "R" -> ("Resistance", SiOhm))
    ),
    "ampere_def" -> Equation.buildFaster(
      "Definition of ampere",
      "I = Q / t",
      Map("I" -> ("Current", Ampere), "Q" -> ("Charge", SiCoulomb), "t" -> ("Time", Second))
    ),
    "resistance_of_wire" -> Equation.buildFaster(
      "Resistance of a conducting wire",
      "R = rho * L / A",
      Map("L" -> ("Length", Meter), "A" -> ("Cross-sectional area", Meter ** 2), "rho" -> ("Resistivity", SiOhm * Meter), "R" -> ("Resistance", SiOhm))
    ),
    "electric_power" -> Equation.buildFaster("Definition of electric power",
      "P = I * V",
      Map("P" -> ("Power", SiDimension.SiWatt), "I" -> ("Current", Ampere), "V" -> ("Voltage", SiVolt))
    ),
    "resistive_dissipation" -> Equation.buildFaster("Resistive dissipation",
      "P = V**2 / R",
      Map("P" -> ("Power", SiDimension.SiWatt), "V" -> ("Voltage", SiDimension.SiVolt), "R" -> ("Resistance", SiOhm))
    ),
    "resistive_dissipation_2" -> Equation.buildFaster("Resistive dissipation",
      "P = I**2 / R",
      Map("P" -> ("Power", SiDimension.SiWatt), "I" -> ("Current", Ampere), "R" -> ("Resistance", SiOhm))
    ),
    "electric_energy" -> Equation.buildFaster("Energy of moving electric charge through a potential",
      "U = V * Q",
      Map("U" -> ("Electrical potential energy", SiDimension.SiJoule), "V" -> ("Potential difference", SiDimension.SiVolt), "Q" -> ("Charge", SiDimension.SiCoulomb))
    ),
    "force_due_to_uniform_potential" -> Equation.buildFaster("Potential due to uniform electric field",
      "V = l * E",
      Map("V" -> ("Potential difference", SiVolt), "l" -> ("Distance", Meter), "E" -> ("Electric field strength" -> SiNewton / SiCoulomb))
    ),
    "mechanical_energy_definition" -> Equation.buildFaster("Work as product of force and displacement",
      "E = F * d",
      Map("E" -> ("Energy", SiJoule), "d" -> ("Displacement", Meter), "F" -> ("Force" -> SiNewton))
    ),
    "uniform_circular_motion" -> Equation.buildFaster("Uniform circular motion",
      "a = v**2 / r",
      Map("a" -> ("Centripital acceleration", Meter / Second / Second), "v" -> ("Speed", Meter/Second), "r" -> ("Radius" -> Meter))
    ),
    "universal_gravitation" -> Equation.buildFaster("Universal gravitation",
      "F = G * m_A * m_B / r**2",
      Map("F" -> ("Gravitational force", SiNewton), "m_A" -> ("First mass", Kilogram), "m_B" -> ("Second mass", Kilogram), "r" -> ("Distance between masses", Meter)),
      "",
      Set(PhysicalConstant.G)
    )
  )

  // unsafe
  @JSExport
  def getByEqId(name: String): Equation = library(name)

  @JSExport
  def relevantEquationIds(searchTerm: String): js.Array[String] = {
    if (searchTerm.filterNot(_.isWhitespace).isEmpty) {
      js.Array()
    } else {
      val searchTerms = searchTerm.toLowerCase.split(' ').toList
      val stuff = library.filter(pair => this.similarity(pair._2.tags, searchTerms)).take(5).keys
      js.Array(stuff.toList :_*)
    }
  }

  def similarity(tags: Set[String], searchTerms: List[String]): Boolean = {
    searchTerms.forall(term => tags.exists(_.contains(term)))
  }
}
