package workspace
import cas.{Expression, RationalNumber}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Gem.EquationLibrary")
object EquationLibrary {
  import Dimension.{Joule,Newton}

  val library: Map[String, LibraryEquation] = Map(
    "ke_def" -> LibraryEquation("Definition of kinetic energy",
      Expression.buildGoofily(RationalNumber(1, 2), Map("E_K" -> -1, "m" -> 1, "v" -> 2)),
      (f: String => BuckTex) => CompileToBuckTex.centeredBox(List(f("E_K"), Text(" = ½"), f("m"), f("v"), Sup(List(Text("2"))))),
      Map("E_K" -> Joule, "m" -> Kilogram, "v" -> Meter / Second),
      Map("E_K" -> "Kinetic energy", "m" -> "Mass", "v" -> "Velocity"),
      Set("kinetic", "energy")),
    "pe_def" -> LibraryEquation("Definition of gravitational potential energy", Expression.buildGoofily(Map("E_P" -> -1, "m" -> 1, "g" -> 1, "h" -> 1)),
      (f: String => BuckTex) => CompileToBuckTex.centeredBox(List(f("E_P"), Text(" = "), f("m"), f("g"), f("h"))),
      Map("E_P" -> Joule, "m" -> Kilogram, "g" -> Meter / (Second ** 2), "h" -> Meter),
      Map("E_P" -> "Potential energy", "m" -> "Mass", "g" -> "Strength of gravity", "h" -> "Height"),
      Set("gravitational", "potential", "energy")
    ),
    "hookes_law" -> Equation.buildQuickly("Hooke's law", ("F", "Spring force", Newton),
      Map("k" -> (1, "Spring constant", Newton / Meter),
        "x" -> (1, "Displacement", Meter)),
      "spring hooke's law",
      RationalNumber(-1)
    ),
    "energy_of_spring" -> Equation.buildQuickly("Energy of spring", ("E_S", "Spring potential energy", Joule),
      Map("k" -> (1, "Spring constant", Newton / Meter),
        "x" -> (2, "Displacement", Meter)),
      "spring hooke's law",
      RationalNumber(1, 2)
    ),
    "force_due_to_gravity" -> Equation.buildQuickly("Force due to gravity", ("F", "Gravitational force", Newton),
      Map("g" -> (1, "Gravitational acceleration", Meter / Second / Second),
        "m" -> (1, "Mass", Kilogram)),
      "force gravity"
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
