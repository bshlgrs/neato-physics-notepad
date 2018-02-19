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
        .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))
        .addEquality(EquationVarId(0, "E_K"), EquationVarId(1, "E_P"))
        .addExpression(EquationVarId(0, "v"))

      assert(ws.expressions(EquationVarId(0, "v")).equivalent((RationalNumber[VarId](2) * Variable[VarId](EquationVarId(0, "E_K")) / Variable[VarId](EquationVarId(0, "m"))).sqrt))

      val ws2 = ws.rewriteExpression(EquationVarId(0, "v"), EquationVarId(0, "E_K"), 1)
    }
  }

  describe("listed actions are exactly correct") {
    val ws = Workspace.empty
      .addEquation(keEquation)
      .addEquation(peEquation)
      .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))

    it("knows about allowed rewrites") {
      val ws2 = ws.addExpression(EquationVarId(0, "v"))

//      assert(ws2.possibleRewritesForExpr(EquationVarId(0, "v")) == Set((EquationVarId(0, "m"), 1)))
    }
  }

  describe("substitution") {
    it("works") {
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))
        .addEquality(EquationVarId(0, "E_K"), EquationVarId(1, "E_P"))
        .addExpression(EquationVarId(0, "v"))
        .rewriteExpression(EquationVarId(0, "v"), EquationVarId(0, "E_K"), 1)

      println(ws.expressions(EquationVarId(0, "v")))
    }

  }

  describe("custom equations") {
    it("works") {
      val ws = Workspace.empty
        .addEquation(EquationParser.parseEquation("KE = 1/2 * m * v**2").get)
        .addEquation(EquationParser.parseEquation("PE = m * g * h").get)
        .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))
        .addEquality(EquationVarId(0, "KE"), EquationVarId(1, "PE"))
        .addExpression(EquationVarId(0, "v"))
        .rewriteExpression(EquationVarId(0, "v"), EquationVarId(0, "KE"), 1)

      assert(ws.expressions(EquationVarId(0, "v")) == (Variable(EquationVarId(1, "g")) * Variable(EquationVarId(1, "h")) * 2).sqrt)
      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }
  }

  describe("regressions") {
    it("does one thing") {
      val E_T = EquationVarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(EquationParser.parseEquation("KE = 1/2 * m * v**2").get)
        .addEquation(EquationParser.parseEquation("PE = m * g * h").get)
        .addEquation(EquationParser.parseEquation("E_T = KE + PE").get)
        .addEquality(EquationVarId(0, "KE"), EquationVarId(2, "KE"))
        .addEquality(EquationVarId(1, "PE"), EquationVarId(2, "PE"))
        .addExpression(E_T)

      assert(ws.expressions(EquationVarId(2, "E_T")) == Variable(EquationVarId(2, "KE")) + Variable(EquationVarId(2, "PE")))

      val ws2 = ws.rewriteExpression(E_T, EquationVarId(0, "KE"), 0)
      assert(ws2.expressions(EquationVarId(2, "E_T")) == Variable(EquationVarId(0, "m")) * (Variable(EquationVarId(0, "v")) ** 2) / 2 + Variable(EquationVarId(1, "PE")))

      ws2.allVarIds.map(ws2.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does another thing") {
      val E_T = EquationVarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquation(EquationParser.parseEquation("E_T = E_K + E_P").get)
        .addEquality(EquationVarId(0, "E_K"), EquationVarId(2, "E_K"))
        .addEquality(EquationVarId(1, "E_P"), EquationVarId(2, "E_P"))
        .addExpression(E_T)

      assert(ws.expressions(EquationVarId(2, "E_T")) == Variable(EquationVarId(2, "E_K")) + Variable(EquationVarId(2, "E_P")))

      val ws2 = ws.rewriteExpression(E_T, EquationVarId(0, "E_K"), 0)
      assert(ws2.expressions(EquationVarId(2, "E_T")) == Variable(EquationVarId(0, "m")) * (Variable(EquationVarId(0, "v")) ** 2) / 2 + Variable(EquationVarId(1, "E_P")))

      val ws3 = ws2.rewriteExpression(E_T, EquationVarId(1, "E_P"), 1)

      val potential_energy_expr = Variable[VarId](EquationVarId(1,"h")) * Variable[VarId](EquationVarId(1,"m")) * Variable[VarId](EquationVarId(1,"g"))
      assert(ws3.expressions(EquationVarId(2, "E_T")).equivalent(Variable[VarId](EquationVarId(0, "m")) * (Variable[VarId](EquationVarId(0, "v")) ** 2) / 2 +
        potential_energy_expr))

      ws3.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a third thing") {
      val E_T = EquationVarId(2, "E_T")
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquation(EquationParser.parseEquation("E_T = E_K + E_P").get)
        .addEquality(EquationVarId(0, "E_K"), EquationVarId(2, "E_K"))
        .addEquality(EquationVarId(1, "E_P"), EquationVarId(2, "E_P"))
        .addExpression(E_T)
        .rewriteExpression(E_T, EquationVarId(0, "E_K"), 0)

      ws.addNumber(PhysicalNumber(5, SiDimension.SiJoule))
          .attachNumber(0, EquationVarId(2, "E_T"))
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("has correct rewrites") {
      val ws = Workspace.empty
        .addEquation(keEquation)
        .addEquation(peEquation)
        .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))
        .addEquality(EquationVarId(0, "E_K"), EquationVarId(1, "E_P"))
        .addExpression(EquationVarId(0, "v"))
        .rewriteExpression(EquationVarId(0, "v"), EquationVarId(0, "E_K"), 1)
      println(ws.expressions)
      println(ws.possibleRewritesForExpr(EquationVarId(0, "v")))
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a fourth thing") {
      val expr = Workspace.empty
        .addEquation(ampereEquation)
        .addExpression(EquationVarId(0, "Q"))
        .addNumber(PhysicalNumber(240, Second))
        .attachNumber(0, EquationVarId(0, "t"))
        .get
        .getExpressionBuckTex(EquationVarId(0, "Q"))

    }

    it("does a fifth thing") {
      val ws = Workspace.empty
        .addEquation(ampereEquation)
        .addEquation(EquationParser.parseEquation("Q = n_e * q_e").get)
        .addExpression(EquationVarId(1, "n_e"))
        .addEquality(EquationVarId(0, "Q"), EquationVarId(1, "Q"))

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)

      println(ws.possibleRewritesForExpr(EquationVarId(1, "n_e")))
      println(ws.checkRewriteAttemptIsValid(EquationVarId(1, "n_e"), EquationVarId(0, "Q"), 0))
    }

    it("does a sixth thing") {
      val ws = Workspace.empty
        .addEquation(springEquation)
        .addEquation(keEquation)
        .addEquality(EquationVarId(0, "E_S"), EquationVarId(1, "E_K"))
        .addExpression(EquationVarId(1, "v"))
        .rewriteExpression(EquationVarId(1, "v"), EquationVarId(1, "E_K"), 0)

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
        .attachNumber(0, EquationVarId(0, "m")).get

      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      assert(ws.getNumber(EquationVarId(0, "m")).contains(num1))

      val ws2 = ws.attachNumber(1, EquationVarId(0, "m")).get
      assert(ws2.getNumber(EquationVarId(0, "m")).contains(num2))
    }

    it("does an eighth thing") {
      val ws = Workspace.empty
        .addEquation(peEquation)
        .addEquation(keEquation)
        .addEquality(EquationVarId(0, "E_P"), EquationVarId(1, "E_K"))

      assert(ws.getDimension(EquationVarId(0, "E_P")).contains(SiDimension.SiJoule))
      ws.allVarIds.map(ws.getDimension)
      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
    }

    it("does a ninth thing") {
      val ws = Workspace.empty
        .addEquation(peEquation)
        .addEquation(keEquation)
        .addEquation(EquationParser.parseEquation("E_A = E_B + E_C").get)
        .addEquality(EquationVarId(0, "E_P"), EquationVarId(2, "E_A"))
        .addEquality(EquationVarId(1, "E_K"), EquationVarId(2, "E_B"))

      ws.getDimension(EquationVarId(2, "E_B"))
//        ws.allVarIds.map(ws.getDimension)
//      ws.allVarIds.map(varId => ws.addExpression(varId).getExpressionBuckTex(varId))
      println(ws)
    }

    it("does a tenth thing") {
      val ws = Workspace.empty
        .addEquation(resistivityEquation)
        .addEquation(keEquation)

      println(ws.getDimensionCalc(EquationVarId(0, "I")))
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
          .addEquality(EquationVarId(0, "E_K"), EquationVarId(1, "E_P"))

      println(ws.getDimensionCalc(EquationVarId(0, "E_K")))

    }

    it("can do it on another case") {
      val realEquation = EquationParser.parseExpression("γ - 1/sqrt(1-v^2/c^2)").get
      val lorentzFactor = LibraryEquation("Lorentz factor", EquationParser.parseExpression("γ - 1/(1 - c/v)").get, 1, (f) => ???,
        Map("γ" -> SiDimension.Dimensionless, "v" -> Meter/Second, "c" -> Meter/Second),
        Map("γ" -> "Lorentz factor", "v" -> "Velocity", "c" -> "Speed of light"),
        Set("c")
      )

      val expr = EquationParser.parseExpression("γ - 1/(1 - c/v)").get.solve("v").head
//      val ws = Workspace.empty.addEquation(lorentzFactor).getDimensionCalc(EquationVarId(0, "v"))
      println(expr)
      println(expr.calculateDimension(
          Map("c" -> Meter / Second, "γ" -> SiDimension.Dimensionless).mapValues(ConcreteDimensionInference))
      )
      assert(false) // fixme
    }
  }

  describe("number inference") {
    it("can do it") {
      val ws = Workspace.empty.addEquation(keEquation)
        .addAndAttachNumber(EquationVarId(0, "E_K"), PhysicalNumber(3, SiDimension.SiJoule))
        .addAndAttachNumber(EquationVarId(0, "v"), PhysicalNumber(5, Meter / Second))
        .addExpression(EquationVarId(0, "m"))
        .addEquation(peEquation)
        .addAndAttachNumber(EquationVarId(1, "g"), PhysicalNumber(7, Meter / Second / Second))
        .addAndAttachNumber(EquationVarId(1, "h"), PhysicalNumber(11, Meter))
        .addEquality(EquationVarId(0, "m"), EquationVarId(1, "m"))
        .addExpression(EquationVarId(1, "E_P"))

      println(ws.recursivelyEvaluatedNumbers)
    }
  }

  describe("diagrams") {
    it("lets you use them") {
      val ws = Workspace.empty.addDiagram.addEquation(peEquation)

      println(ws.allVarIds)
    }
  }
}
