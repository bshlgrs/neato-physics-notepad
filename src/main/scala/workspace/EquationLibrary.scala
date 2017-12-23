package workspace

object EquationLibrary {
  val library = Map[String, Equation](
    "ke_def" -> Equation(Expression[String](0.5, Map("KE" -> -1, "m" -> 1, "v" -> 2))),
    "pe_def" -> Equation(Expression[String](1, Map("PE" -> -1, "m" -> 1, "g" -> 1, "h" -> 1)))
  )
  // unsafe
  def getByEqId(name: String): Equation = library(name)
}
