package workspace

import cas.ExpressionDisplay.{orderWithConstantsFirst}
import cas._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

trait BuckTex {
  @JSExport
  def typeStr: String = this match {
    case _: FlexBox => "FlexBox"
    case _: Sup => "Sup"
    case _: Sub => "Sub"
    case _: Fraction => "Fraction"
    case _: Text => "Text"
    case _: VariableWrapper => "VariableWrapper"
    case _: Surd => "Surd"
  }
}

@JSExportAll
case class FlexDirection(dir: String)
object Column extends FlexDirection("column")
object Row extends FlexDirection("row")

@JSExportAll
case class AlignItemDirection(dir: String)
object FlexEnd extends AlignItemDirection("flex-end")
object Center extends AlignItemDirection("center")

@JSExportAll
case class FlexBox(items: List[BuckTex], flexDirection: FlexDirection, alignItems: AlignItemDirection) extends BuckTex {
  def jsItems: js.Array[BuckTex] = js.Array(items :_*)
}

case class Sup(items: List[BuckTex]) extends BuckTex {
  @JSExport
  def jsItems: js.Array[BuckTex] = js.Array(items :_*)
}
case class Sub(items: List[BuckTex]) extends BuckTex {
  @JSExport
  def jsItems: js.Array[BuckTex] = js.Array(items :_*)
}
case class Fraction(numerator: List[BuckTex], denominator: List[BuckTex]) extends BuckTex {
  @JSExport
  def jsNumerator: js.Array[BuckTex] = js.Array(numerator :_*)
  @JSExport
  def jsDenominator: js.Array[BuckTex] = js.Array(denominator :_*)
}
case class Surd(items: List[BuckTex]) extends BuckTex {
  @JSExport
  def jsItems: js.Array[BuckTex] = js.Array(items :_*)
}
@JSExportAll
case class Text(text: String) extends BuckTex
@JSExportAll
case class VariableWrapper(item: BuckTex, varId: VarId) extends BuckTex

object CompileToBuckTex {
  def horizontalBox(items: List[BuckTex]) = FlexBox(items, Row, FlexEnd)
  def centeredBox(items: List[BuckTex]) = FlexBox(items, Row, Center)

  def compileExpression(expr: Expression[BuckTex]): BuckTex = {
    compileExpressionWithBinding(expr, 0)
  }

  def compileExpressionWithBinding(expr: Expression[BuckTex], strongestPullFromOutside: Int): BuckTex = {
    def wrapIfNeeded(stuff: BuckTex, pullStrengthAtWhichWrappingIsNeeded: Int): BuckTex = {
      if (pullStrengthAtWhichWrappingIsNeeded > strongestPullFromOutside) {
        horizontalBox(List(Text("("), stuff, Text(")")))
      } else {
        stuff
      }
    }
    expr match {
      case Sum(set) => {
        wrapIfNeeded(centeredBox(set.toList.flatMap((x) => {
          val (useMinusSign, positivizedExpression) = extractMinusFromExpression(x)
          List(Text(if (useMinusSign) " - " else " + "), compileExpressionWithBinding(positivizedExpression, 1))
        }).tail), 1)
      }
      case Product(set) =>
        renderFractionGroup(set)
      case Power(lhs, rhs) => centeredBox(List(compileExpressionWithBinding(lhs, strongestPullFromOutside),
        Sup(List(compileExpressionWithBinding(rhs, 0)))
      ))
      case Variable(buckTex) => buckTex
      case RealNumber(r) => Text(r.toString)
      case NamedNumber(_, n, _) => showVarWithStr(n, "")
      case RationalNumber(n, 1) => Text(n.toString)
      case RationalNumber(1, 2) => Text("½")
      case RationalNumber(-1, 2) => Text("-½")
      case RationalNumber(n, d) => Fraction(List(Text(n.toString)), List(Text(d.toString)))
    }
  }

  def showEquation(equation: Equation, idx: Int, varSubscripts: Map[String, Int]): BuckTex = {
    equation.display((varName: String) =>
      makeVariableSpan(VarId(idx, varName), varSubscripts.get(varName)))
  }

  def showVariable(varId: VarId, varSubscripts: Map[VarId, Int]): BuckTex = {
    makeVariableSpan(varId, varSubscripts.get(varId))
  }

  def showVarWithStr(name: String, numStr: String): BuckTex = {
    if (name.contains("_")) {
      var List(mainText, subscript) = name.split('_').toList
      CompileToBuckTex.horizontalBox(List(Text(mainText), Sub(List(Text(subscript + numStr)))))
    } else {
      if (numStr.isEmpty)
        Text(name)
      else
        CompileToBuckTex.horizontalBox(List(Text(name), Sub(List(Text(numStr)))))
    }
  }

  def makeVariableSpan(varId: VarId, mbNum: Option[Int]): BuckTex = {
    val name = varId.varName

    val list = mbNum match {
      case Some(num) => showVarWithStr(name, num.toString)
      case None => showVarWithStr(name, "")
    }
    VariableWrapper(list, varId)
  }

  def showExpression(varId: VarId,
                     expression: Expression[VarId],
                     varSubscripts: Map[VarId, Int],
                     mbNumericValue: Option[PhysicalNumber]): BuckTex = {
    val numericValueDisplay = mbNumericValue match {
      case None => List()
      case Some(PhysicalNumber(numericValue, dimension, _)) =>
        List(Text(s" = ${"%.4g".format(numericValue)}"), dimension.toBuckTex)
    }

    val rhs = expression.mapVariables(varId => makeVariableSpan(varId, varSubscripts.get(varId)))
    val rhsString = this.compileExpression(rhs)

    CompileToBuckTex.centeredBox(List(makeVariableSpan(varId, varSubscripts.get(varId)), Text(" = "),
      rhsString) ++
      numericValueDisplay)
  }

  def renderFractionGroup(set: Set[Expression[BuckTex]]): BuckTex = {
    // TODO: Do something cleverer here: support grouped square roots and fractions
    val denominatorItems: Set[Expression[BuckTex]] = set.collect({ case x@Power(_, RationalNumber(n, _)) if n < 0 => x })
    val numeratorItems = set -- denominatorItems

    val flippedDenominatorItems = denominatorItems.collect(
      { case Power(base, RationalNumber(n, d)) => Expression.makePower(base, RationalNumber(-n, d)): Expression[BuckTex] }
    )

    val numeratorList = renderProductOfPositivePowerTerms(numeratorItems)
    val denominatorList = renderProductOfPositivePowerTerms(flippedDenominatorItems)

    (numeratorList, denominatorList) match {
      case (Nil, Nil) => ???
      case (_, Nil) => centeredBox(numeratorList)
      case (Nil, _) => Fraction(List(Text("1")), denominatorList)
      case _ => Fraction(numeratorList, denominatorList)
    }
  }

  def groupWithRadical[A](items: Set[Expression[A]]): (List[Expression[A]], List[Expression[A]]) = {
    val itemsInsideRadical = items.collect({ case x@Power(_, RationalNumber(1, 2)) => x: Expression[A]})
    val outsideItems = orderWithConstantsFirst(items -- itemsInsideRadical)
    val radicalItems = orderWithConstantsFirst(itemsInsideRadical.collect({ case Power(base, _) => base }))

    outsideItems -> radicalItems
  }

  // assumes these have been sorted
  def renderProductOfPositivePowerTerms(set: Set[Expression[BuckTex]]): List[BuckTex] = {
    def renderAsMinusIfExprIsMinusOne(factor: Expression[BuckTex]):BuckTex = factor match {
      case RationalNumber(-1, 1) => Text("-")
      case _ => compileExpressionWithBinding(factor, 2)
    }

    val (outsideRadical, insideRadical) = groupWithRadical(set)
    val outsideRadicalTex = outsideRadical.map(renderAsMinusIfExprIsMinusOne)
    insideRadical match {
      case Nil => outsideRadicalTex
//      case List(x) => {
//        outsideRadicalTex ++ List(Text("√"), renderAsMinusIfExprIsMinusOne(x))
//      }
//      case _ => {
//        val insideRadicalTex = insideRadical.map(renderAsMinusIfExprIsMinusOne)
//        outsideRadicalTex ++ List(Text("√(")) ++ insideRadicalTex ++ List(Text(")"))
//      }
      case _ => {
        val insideRadicalTex = insideRadical.map(renderAsMinusIfExprIsMinusOne)
        outsideRadicalTex ++ List(Surd(insideRadicalTex))
      }
    }
  }

  def extractMinusFromExpression(expr: Expression[BuckTex]): (Boolean, Expression[BuckTex]) = expr match {
    case Product(factors) => {
      orderWithConstantsFirst(factors).headOption match {
        case Some(x: Constant[_]) if x.evaluate.get < 0 => (true, expr * -1)
        case _ => (false, expr)
      }
    }
    case RationalNumber(n, d) if n < 0 => (true, RationalNumber(-n, d))
    case RealNumber(r) if r < 0 => (true, RealNumber(-r))
    case _ => (false, expr)
  }
}
