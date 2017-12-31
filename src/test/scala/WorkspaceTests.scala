import cas.{EquationParser, Expression, RationalNumber, Variable}
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
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))
      .addEquality(VarId(0, "m"), VarId(1, "m"))

    it("knows about allowed rewrites") {
      val ws2 = ws.addExpression(VarId(0, "v"))

//      assert(ws2.possibleRewritesForExpr(VarId(0, "v")) == Set((VarId(0, "m"), 1)))
    }
  }

  describe("substitution") {
    it("works") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addEquality(VarId(0, "E_K"), VarId(1, "E_P"))
        .addExpression(VarId(0, "v"))
        .rewriteExpression(VarId(0, "v"), VarId(0, "E_K"), 1)

      println(ws.expressions(VarId(0, "v")))
    }

  }

  describe("custom equations") {
    it("works") {
      val ws = Workspace.empty
        .addEquation(EquationParser.parseEquation("KE = 1/2 * m * v**2").get)
        .addEquation(EquationParser.parseEquation("PE = m * g * h").get)
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addEquality(VarId(0, "KE"), VarId(1, "PE"))
        .addExpression(VarId(0, "v"))
        .rewriteExpression(VarId(0, "v"), VarId(0, "KE"), 1)

      assert(ws.expressions(VarId(0, "v")) == (Variable(VarId(1, "g")) * Variable(VarId(1, "h")) * 2).sqrt)
      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }
  }

  describe("regressions") {
    it("does one thing") {
      val E_T = VarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(EquationParser.parseEquation("KE = 1/2 * m * v**2").get)
        .addEquation(EquationParser.parseEquation("PE = m * g * h").get)
        .addEquation(EquationParser.parseEquation("E_T = KE + PE").get)
        .addEquality(VarId(0, "KE"), VarId(2, "KE"))
        .addEquality(VarId(1, "PE"), VarId(2, "PE"))
        .addExpression(E_T)

      assert(ws.expressions(VarId(2, "E_T")) == Variable(VarId(2, "KE")) + Variable(VarId(2, "PE")))

      val ws2 = ws.rewriteExpression(E_T, VarId(0, "KE"), 0)
      assert(ws2.expressions(VarId(2, "E_T")) == Variable(VarId(0, "m")) * (Variable(VarId(0, "v")) ** 2) / 2 + Variable(VarId(1, "PE")))

      ws2.allVarIds.map(ws2.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does another thing") {
      val E_T = VarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquation(EquationParser.parseEquation("E_T = E_K + E_P").get)
        .addEquality(VarId(0, "E_K"), VarId(2, "E_K"))
        .addEquality(VarId(1, "E_P"), VarId(2, "E_P"))
        .addExpression(E_T)

      assert(ws.expressions(VarId(2, "E_T")) == Variable(VarId(2, "E_K")) + Variable(VarId(2, "E_P")))

      val ws2 = ws.rewriteExpression(E_T, VarId(0, "E_K"), 0)
      assert(ws2.expressions(VarId(2, "E_T")) == Variable(VarId(0, "m")) * (Variable(VarId(0, "v")) ** 2) / 2 + Variable(VarId(1, "E_P")))

      val ws3 = ws2.rewriteExpression(E_T, VarId(1, "E_P"), 1)

      val potential_energy_expr = Variable(VarId(1,"h")) * Variable(VarId(1,"m")) * Variable(VarId(1,"g"))
      assert(ws3.expressions(VarId(2, "E_T")) == Variable(VarId(0, "m")) * (Variable(VarId(0, "v")) ** 2) / 2 +
        potential_energy_expr)

      ws3.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a third thing") {
      val E_T = VarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquation(EquationParser.parseEquation("E_T = E_K + E_P").get)
        .addEquality(VarId(0, "E_K"), VarId(2, "E_K"))
        .addEquality(VarId(1, "E_P"), VarId(2, "E_P"))
        .addExpression(E_T)
        .rewriteExpression(E_T, VarId(0, "E_K"), 0)

      ws.addNumber(PhysicalNumber(5, SiDimension.SiJoule))
          .attachNumber(0, VarId(2, "E_T"))
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("has correct rewrites") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addEquality(VarId(0, "E_K"), VarId(1, "E_P"))
        .addExpression(VarId(0, "v"))
        .rewriteExpression(VarId(0, "v"), VarId(0, "E_K"), 1)
      println(ws.expressions)
      println(ws.possibleRewritesForExpr(VarId(0, "v")))
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a fourth thing") {
      val expr = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ampere_def"))
        .addExpression(VarId(0, "Q"))
        .addNumber(PhysicalNumber(240, Second))
        .attachNumber(0, VarId(0, "t"))
        .get
        .getExpressionBuckTex(VarId(0, "Q"))

    }

    it("does a fifth thing") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ampere_def"))
        .addEquation(EquationParser.parseEquation("Q = n_e * q_e").get)
        .addExpression(VarId(1, "n_e"))
        .addEquality(VarId(0, "Q"), VarId(1, "Q"))

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)

      println(ws.possibleRewritesForExpr(VarId(1, "n_e")))
      println(ws.checkRewriteAttemptIsValid(VarId(1, "n_e"), VarId(0, "Q"), 0))
    }

    it("does a sixth thing") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("energy_of_spring"))
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquality(VarId(0, "E_S"), VarId(1, "E_K"))
        .addExpression(VarId(1, "v"))
        .rewriteExpression(VarId(1, "v"), VarId(1, "E_K"), 0)

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)
    }
  }
}
