package workspace

import cas._
import workspace.dimensions.{Dimension, SiDimension}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
sealed trait Equation {
  def expr: Expression[String]
  def display(f: String => BuckTex): BuckTex
  def staticDimensions: Map[String, SiDimension]
  def varName(varSymbol: String): Option[String]
  def varNameJs(varSymbol: String): String = varName(varSymbol).orNull
  def showNaked: BuckTex = display((varName: String) => CompileToBuckTex.makeVariableSpan(VarId(-1, varName), None))
  def vars: Set[String] = expr.vars
  def varsJs: js.Array[String] = js.Array(vars.toList :_*)
  def solve(varName: String, selfEqId: Int): Expression[VarId] = {
    // TODO: check on the `head` here
    expr.solve(varName).head.mapVariables((name) => VarId(selfEqId, name))
  }

  def solutions(varName: String, selfEqId: Int): Set[Expression[VarId]] = {
    expr.solve(varName).map(_.mapVariables((name) => VarId(selfEqId, name)))
  }
  def exprWithEquationId(id: Int): Expression[VarId] = expr.mapVariables((name) => VarId(id, name))

  def toJsObject: js.Object
}


trait EquationJs extends js.Object {
  val id: js.UndefOr[Int]
  val lhs: js.UndefOr[ExpressionJs]
  val rhs: js.UndefOr[ExpressionJs]
}

object EquationJs {
  def parse(equationJs: EquationJs, library: EquationLibrary): Equation = equationJs.id.toOption match {
    case None => CustomEquation(ExpressionJs.parseToStringExpr(equationJs.lhs.get), ExpressionJs.parseToStringExpr(equationJs.rhs.get))
    case Some(id) => library.getByEqId(id)
  }
}

@JSExportAll
case class LibraryEquation(name: String,
                           expr: Expression[String],
                           id: Int,
                           displayF: (String => BuckTex) => BuckTex,
                           staticDimensions: Map[String, SiDimension],
                           varNamesMap: Map[String, String],
                           extraTags: Set[String]
                   ) extends Equation {
  assert(expr.vars == staticDimensions.keys.toSet, s"assert 234876 $name ${expr.vars} ${staticDimensions.keys}")
  assert(varNamesMap.keys == staticDimensions.keys, s"12387340 ${varNamesMap.keys} ${staticDimensions.keys}")

  def varName(symbol: String): Option[String] = varNamesMap.get(symbol)
  def staticDimensionsJs: js.Dictionary[SiDimension] = js.Dictionary(staticDimensions.toSeq :_*)
  def display(f: String => BuckTex): BuckTex = displayF(f)
  def tags: Set[String] = (extraTags ++ varNamesMap.values ++ name.split(' ').toSet).map(_.toLowerCase)

  def toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "LibraryEquation",
    "id" -> id
  )
}

trait LibraryEquationJs extends js.Object {
  val name: String
  val id: Int
  val eqString: String
  val varNames: js.Dictionary[String]
  val dimensionStrings: js.Dictionary[String]
  val constantsUsed: js.Array[String]
}

object LibraryEquationJs {
  def parse(ljs: LibraryEquationJs): LibraryEquation = {
    val nakedEquation = EquationParser.parseEquation(ljs.eqString).getOrElse({
      throw new RuntimeException(s"Error thrown while trying to parse ${ljs.name} ${ljs.eqString}")})

    def removeConstants(expr: Expression[String]): Expression[String] = expr.mapVariablesToExpressions[String]((varName: String) => {
      if (ljs.constantsUsed.contains(varName) || varName == "π") {
        PhysicalConstant.constants.find(_.namedNumber.name == varName) match {
          case None => throw new RuntimeException(s"Tried to use physical constant $varName, which doesn't exist")
          case Some(x) => x.namedNumber
        }
      } else
        Variable(varName)
    })

    val exprWithoutConstants = removeConstants(nakedEquation.expr)

    def display(f: (String => BuckTex)): BuckTex = {
      CompileToBuckTex.centeredBox(List(
        CompileToBuckTex.compileExpression(removeConstants(nakedEquation.lhs).mapVariables(f)), Text(" = "),
        CompileToBuckTex.compileExpression(removeConstants(nakedEquation.rhs).mapVariables(f))))
    }

    LibraryEquation(
      ljs.name,
      exprWithoutConstants,
      ljs.id,
      display,
      ljs.dimensionStrings.toMap.mapValues((dimStr: String) => Dimension.parse(dimStr).get.siDimension),
      ljs.varNames.toMap,
      Set()
    )
  }
}

@JSExportAll
case class CustomEquation(lhs: Expression[String], rhs: Expression[String]) extends Equation {
  def expr: Expression[String] = lhs - rhs

  def varName(varSymbol: String): Option[String] = None

  def display(f: String => BuckTex): BuckTex =
    CompileToBuckTex.centeredBox(List(CompileToBuckTex.compileExpression(lhs.mapVariables(f)),
                                     Text("="),
                                     CompileToBuckTex.compileExpression(rhs.mapVariables(f))))

  def staticDimensions = Map()

  def toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "CustomEquation",
    "lhs" -> lhs.toJsObject,
    "rhs" -> rhs.toJsObject
  )
}
