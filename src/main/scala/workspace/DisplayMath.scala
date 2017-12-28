package workspace

import cas._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportAll
case class DisplayMath(stuff: List[DisplayMathElement]) {
  def ++(other: DisplayMath): DisplayMath = DisplayMath(this.stuff ++ other.stuff)
  def ++(other: DisplayMathElement): DisplayMath = DisplayMath(this.stuff :+ other)

  def jsItems: js.Array[DisplayMathElement] = js.Array(stuff :_*)
}

trait DisplayMathElement {
  @JSExport
  def name: String = this match {
    case _: Span => "span"
    case _: VariableSpan => "variableSpan"
    case _: Sup => "sup"
    case _: Sub => "sub"
    case _: Fraction => "fraction"
    case _: Box => "box"
  }
}

@JSExportAll
case class Span(str: String) extends DisplayMathElement

@JSExportAll
case class VariableSpan(varId: VarId, el: List[DisplayMathElement]) extends DisplayMathElement {
  def jsEls: js.Array[DisplayMathElement] = js.Array(el :_*)
}
@JSExportAll
case class Fraction(numerator: DisplayMath, denominator: DisplayMath) extends DisplayMathElement

@JSExportAll
case class Box(stuff: DisplayMath) extends DisplayMathElement

case class Sup(inner: List[DisplayMathElement]) extends DisplayMathElement {
  @JSExport
  def jsInner: js.Array[DisplayMathElement] = js.Array(inner :_*)
}
object Sup {
  def apply(x: DisplayMathElement): Sup = Sup(List(x))
  def apply(x: DisplayMath): Sup = Sup(x.stuff)
}
case class Sub(inner: List[DisplayMathElement]) extends DisplayMathElement {
  @JSExport
  def jsInner: js.Array[DisplayMathElement] = js.Array(inner :_*)
}


@JSExportTopLevel("Gem.DisplayMath")
object DisplayMath {
  def apply(str: String): DisplayMath = DisplayMath(List(Span(str)))
  def apply(el: DisplayMathElement): DisplayMath = DisplayMath(List(el))

  def render(expr: Expression[VariableSpan]): DisplayMath = {
    this.renderWithBinding(expr)._1
  }

  def join(things: List[DisplayMath], thing: String): DisplayMath = {
    def intersperse[A](list: List[List[A]], sep: A): List[A] = list.flatMap((x) => List(sep) ++ x).tail
    // bleh
    DisplayMath(intersperse(things.map(_.stuff), Span(thing)))
  }

  def wrap(tuple: (DisplayMath, Int), binding: Int): DisplayMath = {
    if (tuple._2 >= binding) tuple._1 else DisplayMath("(") ++ tuple._1 ++ DisplayMath(")")
  }

  def renderWithBinding(expr: Expression[VariableSpan]): (DisplayMath, Int) = {
    expr match {
      case Sum(set) => join(set.toList.map((x) => wrap(this.renderWithBinding(x), 0)), " + ") -> 0
      case Product(set) => {
        ???
      }
//        ExpressionDisplay.fractionDisplay(set, " ", (x: Expression[String]) => x.toStringWithBinding, (x) => s"√($x)", (n, d) => s"($n)/($d)") -> 1
//        DisplayMath(Fraction(join(set.toList.map((x) => DisplayMath(Box(wrap(this.renderWithBinding(x), 0)))), " * "), DisplayMath("lol"))) -> 0
      case Power(lhs, rhs) => wrap(this.renderWithBinding(lhs), 2) ++ Sup(render(rhs)) -> 0
      case Variable(varSpan) => DisplayMath(List(varSpan)) -> 3
      case RealNumber(r) => DisplayMath(r.toString) -> 3
      case RationalNumber(1, 2) => DisplayMath("½") -> 3
      case RationalNumber(1, 3) => DisplayMath("⅓") -> 3
      case RationalNumber(2, 3) => DisplayMath("⅔") -> 3
      case RationalNumber(1, 4) => DisplayMath("¼") -> 3
      case RationalNumber(n, 1) => DisplayMath(n.toString) -> 3
      case RationalNumber(n, d) => DisplayMath(s"$n/$d") -> 1
    }
  }

  def makeVariableSpan(varId: VarId, mbNum: Option[Int]): VariableSpan = {
    val name = varId.varName

    def showVarWithStr(numStr: String): List[DisplayMathElement] = {
      if (name.contains("_")) {
        var List(mainText, subscript) = name.split('_').toList
        List(Span(mainText), Sub(List(Span(subscript + numStr))))
      } else {
        if (numStr.isEmpty)
          List(Span(name))
        else
          List(Span(name), Sub(List(Span(numStr))))
      }
    }

    val list = mbNum match {
      case Some(num) => showVarWithStr(num.toString)
      case None => showVarWithStr("")
    }
    VariableSpan(varId, list)
  }

  def showEquation(equation: Equation, equationIdx: Int, varSubscripts: Map[String, Int]): DisplayMath = {
    equation.display((varName: String) => makeVariableSpan(VarId(equationIdx, varName), varSubscripts.get(varName)))
  }

  @JSExport
  def showNakedEquation(equation: Equation): DisplayMath = {
    equation.display((varName: String) => makeVariableSpan(VarId(0, varName), None))
  }

  def showExpression(varId: VarId,
                     expression: Expression[VarId],
                     varSubscripts: Map[VarId, Int],
                     mbNumericValue: Option[PhysicalNumber]): DisplayMath = {
    val numericValueDisplay = mbNumericValue match {
      case None => DisplayMath(List())
      case Some(PhysicalNumber(numericValue, dimension)) =>
        DisplayMath(List(Span(s" = ${numericValue.toString.take(5)}")) ++ dimension.toDisplayMath.stuff)
    }

    DisplayMath(List(makeVariableSpan(varId, varSubscripts.get(varId)), Span(" = "))) ++
      render(expression.mapVariables(varId => makeVariableSpan(varId, varSubscripts.get(varId)))) ++
      numericValueDisplay
  }

  def showVariable(varId: VarId, varSubscripts: Map[VarId, Int]): DisplayMath = {
    DisplayMath(List(makeVariableSpan(varId, varSubscripts.get(varId))))
  }
}
