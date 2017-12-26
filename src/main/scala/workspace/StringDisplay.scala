package workspace

import cas.Expression

object StringDisplay {
  def addSubscripts(eq: String, numbers: Map[String, Int]) ={
    ???
  }

  def showVar(name: String, mbNum: Option[Int]): String = {
    def showVarWithStr(numStr: String) = {
      if (name.contains("_")) {
        var List(mainText, subscript) = name.split('_').toList
        s"$mainText<sub>$subscript$numStr</sub>"
      } else {
        if (numStr.isEmpty)
          name
        else
          s"$name<sub>$numStr</sub>"
      }
    }
    mbNum match {
      case Some(num) => showVarWithStr(num.toString)
      case None => showVarWithStr("")
    }
  }

//  def showEquation(eq: Equation, varSubscripts: Map[String, Int]): String = {
//    val regex = raw"\{([^}]+)\}".r("varName")
//    regex.replaceAllIn(eq.latexString, _ match {
//      case regex(varName) => StringDisplay.showVar(varName, varSubscripts.get(varName))
//    })
//  }

  def showExpression(exprVarId: VarId, expr: Expression[VarId], varSubscripts: Map[VarId, Int]): String = {
    val lhs = StringDisplay.showVar(exprVarId.varName, varSubscripts.get(exprVarId))
    val rhs = expr.mapVariables((varId) => StringDisplay.showVar(varId.varName, varSubscripts.get(varId))).toString
    s"$lhs = $rhs"
  }
}
