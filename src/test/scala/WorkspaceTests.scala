import cas.{Expression, RationalNumber, Variable}
import org.scalatest.FunSpec
import workspace._

class WorkspaceTests extends FunSpec {
  import RationalNumber._
  describe("sequences of actions") {
    it("can do the ball-down-a-slide one") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addEquality(VarId(0, "E_K"), VarId(1, "E_P"))
        .addExpression(VarId(0, "v"))

      assert(ws.exprs(VarId(0, "v")) == (RationalNumber(2) * Variable(VarId(0, "E_K")) / Variable(VarId(0, "m"))).sqrt)

      val ws2 = ws.rewriteExpression(VarId(0, "v"), VarId(0, "E_K"), 1)
//      assert(ws2.exprs((0, "v")) == (RationalNumber(2) * Variable(1 -> "g") * Variable(1 -> "h")).sqrt)
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))
      .addEquality(VarId(0, "m"), VarId(1, "m"))

    it("has them all right") {
      assert(ws.possibleActions == Set(
        AddEqualityAction(VarId(0, "E_K"), VarId(1, "E_P")),
        AddEqualityAction(VarId(1, "E_P"), VarId(0, "E_K")),
        AddExpressionAction(VarId(0, "E_K")),
        AddExpressionAction(VarId(0, "m")),
        AddExpressionAction(VarId(0, "v")),
        AddExpressionAction(VarId(1, "E_P")),
        AddExpressionAction(VarId(1, "m")),
        AddExpressionAction(VarId(1, "g")),
        AddExpressionAction(VarId(1, "h")),
        RemoveEqualityAction(VarId(0, "m")),
        RemoveEqualityAction(VarId(1, "m")),
      ))
    }

    it("can handle all the allowed expression rewriting possibilities") {
      val ws2 = ws.addExpression(VarId(0, "v"))
      assert(ws2.possibleActions.filter(_.isInstanceOf[RewriteExpressionAction]) ==
        Set(RewriteExpressionAction(VarId(0, "v"), VarId(0, "E_K"),0), RewriteExpressionAction(VarId(0, "v"), VarId(0, "m"),0)))
    }

    it("can handle attaching numbers") {
      val ws2 = ws.addNumber(PhysicalNumber(9.8, Meter / Second ** 2))
        .addNumber(PhysicalNumber(12, Dimension.Joule))

      assert(ws2.possibleActions.filter(_.isInstanceOf[AttachNumberAction]) ==
        Set(
          AttachNumberAction(0, VarId(1, "g")),
          AttachNumberAction(1, VarId(1, "E_P")),
          AttachNumberAction(1, VarId(0, "E_K"))
        ))

      val ws3 = ws2.attachNumber(0, VarId(1, "g")).get

      assert(ws3.possibleActions.filter(_.isInstanceOf[DetachNumberAction]) ==
        Set(DetachNumberAction(0)))
    }
  }

  describe("subscript generation") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))

    it("displays equations reasonably") {
      println(ws.showEquation(0))
      println(ws.showEquation(1))
      assert(ws.showEquation(0) == """E_K = \frac12 m_{1} v^2""")
      assert(ws.showEquation(1) == "E_P = m_{2} g h")
    }

    it("understands equalities") {
      val ws2 = ws.addEquality(VarId(0, "m"), VarId(1, "m"))

      assert(ws2.showEquation(0) == """E_K = \frac12 m v^2""")
      assert(ws2.showEquation(1) == "E_P = m g h")
    }

    it("can show expressions") {
      // TODO

      /// these are gonna be flaky because there are multiple reasonable things to do, and I'm just hardcoding one I like
    }
  }
}
