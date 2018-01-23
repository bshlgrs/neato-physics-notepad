import cas.{EquationParser, Expression, RationalNumber, Variable}
import org.scalatest.FunSpec
import workspace._
import workspace.dimensions._

class WorkspaceTests extends FunSpec {
  val keEquation = LibraryEquation("Kinetic energy", EquationParser.parseExpression("E_K - 1/2*m*v**2").get, 1, (f) => ???,
    Map("E_K" -> SiDimension.SiJoule, "m" -> Kilogram, "v" -> Meter/Second),
    Map("E_K" -> "Kinetic energy", "m" -> "Mass", "v" -> "Velocity"),
    Set()
  )
  val peEquation = LibraryEquation("Potential energy", EquationParser.parseExpression("E_P - m*g*h").get, 2, (f) => ???,
    Map("E_P" -> SiDimension.SiJoule, "m" -> Kilogram, "g" -> Meter / Second / Second, "h" -> Meter),
    Map("E_P" -> "Potential energy", "m" -> "Mass", "g" -> "Gravity", "h" -> "Height"),
    Set()
  )
  val ampereEquation = LibraryEquation("Ampere definition", EquationParser.parseExpression("Q - I * t").get, 3, (f) => ???,
    Map("Q" -> SiDimension.SiCoulomb, "I" -> Ampere, "t" -> Second),
    Map("Q" -> "Charge", "I" -> "Current", "t" -> "Time"),
    Set()
  )
  val springEquation = LibraryEquation("Spring energy", EquationParser.parseExpression("E_S - 1/2*k*x**2").get, 4, (f) => ???,
    Map("E_S" -> SiDimension.SiJoule, "k" -> SiDimension.SiNewton/Meter, "x" -> Meter),
    Map("E_S" -> "Spring energy", "k" -> "Spring constant", "x" -> "Displacement"),
    Set()
  )

  val resistivityEquation = LibraryEquation("Resistivity", EquationParser.parseExpression("P - I**2*R").get, 3, (f) => ???,
    Map("P" -> SiDimension.SiWatt, "I" -> Ampere, "R" -> SiDimension.SiOhm),
    Map("P" -> "Power", "I" -> "Current", "R" -> "Resistance"),
    Set()
  )

  import RationalNumber._
  describe("sequences of actions") {
    it("can do the ball-down-a-slide one") {
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addEquality(VarId(0, "E_K"), VarId(1, "E_P"))
        .addExpression(VarId(0, "v"))

      assert(ws.expressions(VarId(0, "v")).equivalent((RationalNumber(2) * Variable(VarId(0, "E_K")) / Variable(VarId(0, "m"))).sqrt))

      val ws2 = ws.rewriteExpression(VarId(0, "v"), VarId(0, "E_K"), 1)
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(keEquation)
      .addEquation(peEquation)
      .addEquality(VarId(0, "m"), VarId(1, "m"))

    it("knows about allowed rewrites") {
      val ws2 = ws.addExpression(VarId(0, "v"))

//      assert(ws2.possibleRewritesForExpr(VarId(0, "v")) == Set((VarId(0, "m"), 1)))
    }
  }

  describe("substitution") {
    it("works") {
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
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
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquation(EquationParser.parseEquation("E_T = E_K + E_P").get)
        .addEquality(VarId(0, "E_K"), VarId(2, "E_K"))
        .addEquality(VarId(1, "E_P"), VarId(2, "E_P"))
        .addExpression(E_T)

      assert(ws.expressions(VarId(2, "E_T")) == Variable(VarId(2, "E_K")) + Variable(VarId(2, "E_P")))

      val ws2 = ws.rewriteExpression(E_T, VarId(0, "E_K"), 0)
      assert(ws2.expressions(VarId(2, "E_T")) == Variable(VarId(0, "m")) * (Variable(VarId(0, "v")) ** 2) / 2 + Variable(VarId(1, "E_P")))

      val ws3 = ws2.rewriteExpression(E_T, VarId(1, "E_P"), 1)

      val potential_energy_expr = Variable(VarId(1,"h")) * Variable(VarId(1,"m")) * Variable(VarId(1,"g"))
      assert(ws3.expressions(VarId(2, "E_T")).equivalent(Variable(VarId(0, "m")) * (Variable(VarId(0, "v")) ** 2) / 2 +
        potential_energy_expr))

      ws3.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a third thing") {
      val E_T = VarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
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
        .addEquation(keEquation)
        .addEquation(peEquation)
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
        .addEquation(ampereEquation)
        .addExpression(VarId(0, "Q"))
        .addNumber(PhysicalNumber(240, Second))
        .attachNumber(0, VarId(0, "t"))
        .get
        .getExpressionBuckTex(VarId(0, "Q"))

    }

    it("does a fifth thing") {
      val ws = Workspace.empty
        .addEquation(ampereEquation)
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
        .addEquation(springEquation)
        .addEquation(keEquation)
        .addEquality(VarId(0, "E_S"), VarId(1, "E_K"))
        .addExpression(VarId(1, "v"))
        .rewriteExpression(VarId(1, "v"), VarId(1, "E_K"), 0)

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)
    }

    it("does a seventh thing") {
      val num1 = PhysicalNumber(10, Kilogram)
      val num2 = PhysicalNumber(20, Kilogram)
      val ws = Workspace.empty
        .addEquation(springEquation)
        .addNumber(num1)
        .addNumber(num2)
        .attachNumber(0, VarId(0, "m")).get

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      assert(ws.getNumber(VarId(0, "m")).contains(num1))

      val ws2 = ws.attachNumber(1, VarId(0, "m")).get
      assert(ws2.getNumber(VarId(0, "m")).contains(num2))
    }

    it("does an eighth thing") {
      val ws = Workspace.empty
        .addEquation(peEquation)
        .addEquation(keEquation)
        .addEquality(VarId(0, "E_P"), VarId(1, "E_K"))

      assert(ws.getDimension(VarId(0, "E_P")).contains(SiDimension.SiJoule))
      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a ninth thing") {
      val ws = Workspace.empty
        .addEquation(peEquation)
        .addEquation(keEquation)
        .addEquation(EquationParser.parseEquation("E_A = E_B + E_C").get)
        .addEquality(VarId(0, "E_P"), VarId(2, "E_A"))
        .addEquality(VarId(1, "E_K"), VarId(2, "E_B"))

      ws.getDimension(VarId(2, "E_B"))
//        ws.allVarIds.map(ws.getDimension)
//      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)
    }

    it("does a tenth thing") {
      val ws = Workspace.empty
        .addEquation(resistivityEquation)
        .addEquation(keEquation)

      println(ws.getDimensionCalc(VarId(0, "I")))
      //        ws.allVarIds.map(ws.getDimension)
      //      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)
    }
  }

  describe("changing number dimensions") {
    it("can do it") {
      val ws = Workspace.empty.addNumber(PhysicalNumber.parsePhysicalNumber("5cm").get)

      println(ws.numbers(0)._1.toBuckTex)

      println(ws.changeDimension(0, Dimension.parse("mm").get).numbers(0)._1.toBuckTex)
    }
  }

  describe("dimension inference") {
    it("can do it") {
      val ws = Workspace.empty.addEquation(keEquation)
        .addEquation(EquationParser.parseEquation("E_P = m * g * h").get)
          .addEquality(VarId(0, "E_K"), VarId(1, "E_P"))

      println(ws.getDimensionCalc(VarId(0, "E_K")))

    }
  }

  describe("number inference") {
    it("can do it") {
      val ws = Workspace.empty.addEquation(keEquation)
        .addAndAttachNumber(VarId(0, "E_K"), PhysicalNumber(3, SiDimension.SiJoule))
        .addAndAttachNumber(VarId(0, "v"), PhysicalNumber(5, Meter / Second))
        .addExpression(VarId(0, "m"))
        .addEquation(peEquation)
        .addAndAttachNumber(VarId(1, "g"), PhysicalNumber(7, Meter / Second / Second))
        .addAndAttachNumber(VarId(1, "h"), PhysicalNumber(11, Meter))
        .addEquality(VarId(0, "m"), VarId(1, "m"))
        .addExpression(VarId(1, "E_P"))

      println(ws.recursivelyEvaluatedNumbers)
    }
  }

  describe("diagrams") {
    it("lets you use them") {

    }
  }
}
