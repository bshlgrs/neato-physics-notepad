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

      assert(ws.expressions(VarId(0, "v")) == (RationalNumber(2) * Variable(VarId(0, "E_K")) / Variable(VarId(0, "m"))).sqrt)

      val ws2 = ws.rewriteExpression(VarId(0, "v"), VarId(0, "E_K"), 1)
//      assert(ws2.exprs((0, "v")) == (RationalNumber(2) * Variable(1 -> "g") * Variable(1 -> "h")).sqrt)
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))
      .addEquality(VarId(0, "m"), VarId(1, "m"))

    it("knows about allowed rewrites") {
      val ws2 = ws.addExpression(VarId(0, "v"))

      assert(ws2.possibleRewritesForExpr(VarId(0, "v")) == Set((VarId(0, "m"), 1)))
    }


  }

//  describe("subscript generation") {
//    val ws = Workspace.empty
//      .addEquation(EquationLibrary.getByEqId("ke_def"))
//      .addEquation(EquationLibrary.getByEqId("pe_def"))
//
//    it("displays equations reasonably") {
//      println(ws.showEquation(0))
//      println(ws.showEquation(1))
//      assert(ws.showEquation(0) == """E_K = \frac12 m_{1} v^2""")
//      assert(ws.showEquation(1) == "E_P = m_{2} g h")
//    }
//
//    it("understands equalities") {
//      val ws2 = ws.addEquality(VarId(0, "m"), VarId(1, "m"))
//
//      assert(ws2.showEquation(0) == """E_K = \frac12 m v^2""")
//      assert(ws2.showEquation(1) == "E_P = m g h")
//    }
//
//    it("can show expressions") {
//      val ws2 = ws.addExpression(VarId(0, "v"))
//
//      println(ws2.showExpression(VarId(0, "v")))
//      println(ws2.expressions(VarId(0, "v")).toString)
//      // TODO
//
//      /// these are gonna be flaky because there are multiple reasonable things to do, and I'm just hardcoding one I like
//    }
//  }
}
