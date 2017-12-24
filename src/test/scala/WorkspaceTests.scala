import org.scalatest.FunSpec
import workspace._

class WorkspaceTests extends FunSpec {
  val SQRT_2 = 1.4142135623730951
  describe("sequences of actions") {
    it("can do the ball-down-a-slide one") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquality((0, "m"), (1, "m"))
        .addEquality((0, "KE"), (1, "PE"))
        .addExpression((0, "v"))

      assert(ws.exprs((0, "v")) == Expression(SQRT_2, Map((0, "KE") -> 0.5, (0, "m") -> -0.5)))

      val ws2 = ws.rewriteExpression((0, "v"), (0, "KE"), 1)
      assert(ws2.exprs((0, "v")) == Expression(SQRT_2, Map((1, "g") -> 0.5, (1, "h") -> 0.5)))
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))
      .addEquality((0, "m"), (1, "m"))

    it("has them all right") {
      assert(ws.possibleActions == Set(
        AddEqualityAction((0, "KE"), (1, "PE")),
        AddEqualityAction((1, "PE"), (0, "KE")),
        AddExpressionAction((0, "KE")),
        AddExpressionAction((0, "m")),
        AddExpressionAction((0, "v")),
        AddExpressionAction((1, "PE")),
        AddExpressionAction((1, "m")),
        AddExpressionAction((1, "g")),
        AddExpressionAction((1, "h")),
        RemoveEqualityAction((0, "m")),
        RemoveEqualityAction((1, "m")),
      ))
    }

    it("can handle all the allowed expression rewriting possibilities") {
      val ws2 = ws.addExpression((0, "v"))
      assert(ws2.possibleActions.filter(_.isInstanceOf[RewriteExpressionAction]) ==
        Set(RewriteExpressionAction((0, "v"),(0, "KE"),0), RewriteExpressionAction((0, "v"),(0, "m"),0)))
    }

    it("can handle attaching numbers") {
      val ws2 = ws.addNumber(PhysicalNumber(9.8, Meter / Second ** 2))
        .addNumber(PhysicalNumber(12, Dimension.Joule))

      assert(ws2.possibleActions.filter(_.isInstanceOf[AttachNumberAction]) ==
        Set(
          AttachNumberAction(0, (1, "g")),
          AttachNumberAction(1, (1, "PE")),
          AttachNumberAction(1, (0, "KE"))
        ))

      val ws3 = ws2.attachNumber(0, (1, "g")).get

      assert(ws3.possibleActions.filter(_.isInstanceOf[DetachNumberAction]) ==
        Set(DetachNumberAction(0)))
    }
  }

  describe("subscript generation") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))

    it("displays equations reasonably") {
      assert(ws.showEquation(0) == "E_K = 1/2 m_1 v^2")
      assert(ws.showEquation(1) == "E_P = m_2 g h")
    }

    it("understands equalities") {
      val ws2 = ws.addEquality((0, "m"), (1, "m"))
      assert(ws.showEquation(0) == "E_K = 1/2 m v^2")
      assert(ws.showEquation(1) == "E_P = m g h")
    }

    it("can show expressions") {
      // TODO
    }
  }
}
