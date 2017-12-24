package workspace

import cas.Expression

object LatexString {
  def addSubscripts(eq: String, numbers: Map[String, Int]) ={
    ???
  }

  def showVar(name: String, mbNum: Option[Int]): String = mbNum match {
    case Some(num) => {
      if (name.contains("_")) {
        var List(mainText, subscript) = name.split('_').toList
        s"${mainText}_{$subscript$num}"
      } else
        s"${name}_{$num}"
    }
    case None => name
  }

  def showEquation(eq: Equation, varSubscripts: Map[String, Int]): String = {
    //eq.expr.mapVariables((varName) => LatexString.showVar(varName, varSubscripts.get(varName))).toString

    val regex = raw"\{([^}]+)\}".r("varName")
    regex.replaceAllIn(eq.latexString, _ match {
      case regex(varName) => LatexString.showVar(varName, varSubscripts.get(varName))
    })
  }

  def showExpression(exprVarId: VarId, expr: Expression[VarId], varSubscripts: Map[VarId, Int]): String = {
    val lhs = LatexString.showVar(exprVarId.varName, varSubscripts.get(exprVarId))
    val rhs = expr.mapVariables((varId) => LatexString.showVar(varId.varName, varSubscripts.get(varId))).toString
    s"$lhs = $rhs"
  }
}
