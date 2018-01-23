package workspace

import cas.{Expression, Variable}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

case class TriangleDiagram(vars: Map[String, Either[VarId, String]]) {
  // Triangle
  //
  //     C
  //    /|
  //   / |
  //  /  |
  // A---B
  // theta is the angle CAB, phi is the angle ACB
  // H is hypotenuse AC
  // A is AB
  // O is BC
  def usableEquations(nextEquationIdx: Int): Set[CustomEquation] = {
    val O = Variable("O")
    val A = Variable("A")
    val H = Variable("H")
    val theta = Variable("theta")
    val phi = Variable("phi")

    val identities = Set[(Expression[String], Expression[String])](
      (O**2) + (A**2) -> (H**2),
      O -> H * cas.SpecialFunction.sin(theta),
      A -> H * cas.SpecialFunction.cos(theta),
      A -> H * cas.SpecialFunction.sin(phi),
      O -> H * cas.SpecialFunction.cos(phi),
      // tangents
      O -> A * cas.SpecialFunction.tan(theta),
      A -> O * cas.SpecialFunction.tan(phi),
    )

    val varNames = vars.mapValues({
      case Left(VarId(_, name)) => name
      case Right(name) => name
    })

    val varToEqualitiesMap: Map[String, (VarId, VarId)] = vars.map({
      case (name, Left(varId)) => Some(name -> (VarId(nextEquationIdx, name), varId))
      case (_, Right(name)) => None
    }).collect({ case Some(x) => x }).toMap

    identities
      .filter({ case (lhs, rhs) => (lhs.vars ++ rhs.vars).forall(vars.contains) })
      .map({ case (lhs, rhs) => {
        val builtInEqualities = (lhs.vars ++ rhs.vars).map(varToEqualitiesMap.get)
                                                        .collect({ case Some(v) => v })
        CustomEquation(lhs.mapVariables(varNames), rhs.mapVariables(varNames), builtInEqualities)
      }})
  }

  @JSExport
  def usableEquationsJs(nextEquationIdx: Int): js.Array[CustomEquation] = js.Array(usableEquations(nextEquationIdx).toSeq :_*)

  def set(varName: String, mbSetting: Option[Either[VarId, String]]): TriangleDiagram = mbSetting match {
    case None => this.copy(vars - varName)
    case Some(setting) => this.copy(vars + (varName -> setting))
  }                                     
}
