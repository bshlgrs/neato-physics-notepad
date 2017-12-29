package workspace

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
    case _: Wrapper => "Wrapper"
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
@JSExport
case class Text(text: String) extends BuckTex
@JSExport
case class Wrapper(item: BuckTex, data: js.Dictionary[js.Any] = js.Dictionary()) extends BuckTex

object CompileToBuckTex {
  def horizontalBox(items: List[BuckTex]) = FlexBox(items, Row, FlexEnd)
  def centeredBox(items: List[BuckTex]) = FlexBox(items, Row, Center)

  type VarStuff = (BuckTex, js.Dictionary[js.Any])
  def compileExpression(expr: Expression[VarStuff]): BuckTex = {
    compileExpressionWithBinding(expr, 0)
  }

  def compileExpressionWithBinding(expr: Expression[VarStuff], strongestPullFromOutside: Int): BuckTex = {
    def wrapIfNeeded(stuff: BuckTex, pullStrengthAtWhichWrappingIsNeeded: Int): BuckTex = {
      if (pullStrengthAtWhichWrappingIsNeeded > strongestPullFromOutside) {
        horizontalBox(List(stuff))
      } else {
        stuff
      }
    }
    expr match {
      case Sum(set) => {
        wrapIfNeeded(horizontalBox(set.toList.flatMap((x) => List(Text(" + "), compileExpressionWithBinding(x, 1))).tail),
          1)
      }
      case Product(set) => {
        wrapIfNeeded(horizontalBox(set.toList.flatMap((x) => List(Text(" + "), compileExpressionWithBinding(x, 1))).tail),
          2)
      }
      case Power(lhs, rhs) => horizontalBox(List(compileExpressionWithBinding(lhs, strongestPullFromOutside),
        Sup(List(compileExpressionWithBinding(rhs, 0)))
      ))
      case Variable((buckTex, data)) => Wrapper(buckTex, data)
      case RealNumber(r) => Text(r.toString)
      case RationalNumber(n, 1) => Text(n.toString)
      case RationalNumber(n, d) => Fraction(List(Text(n.toString)), List(Text(d.toString)))
    }
  }
}
