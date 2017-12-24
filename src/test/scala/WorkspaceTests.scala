import org.scalatest.FunSpec
import workspace._

class WorkspaceTests extends FunSpec {
  val SQRT_2 = 1.4142135623730951
  describe("sequences of actions") {
    it("can do the ball-down-a-slide one") {
      val ws = Workspace.empty
        .addEquation(EquationLibrary.getByEqId("ke_def"))
        .addEquation(EquationLibrary.getByEqId("pe_def"))
        .setEquality((0, "m"), (1, "m"))
        .setEquality((0, "KE"), (1, "PE"))
        .solve((0, "v"))

      assert(ws.exprs((0, "v")) == Expression(SQRT_2, Map((0, "KE") -> 0.5, (0, "m") -> -0.5)))

      val ws2 = ws.subExpr((0, "v"), (0, "KE"), 1)
      assert(ws2.exprs((0, "v")) == Expression(SQRT_2, Map((1, "g") -> 0.5, (1, "h") -> 0.5)))
    }
  }

  describe("listed actions are exactly correct") {
    // setup here

    describe("for equalities") {
      it("creation") {
        // disallow redundant ones
      }

      it("destruction") {

      }
    }

    describe("for expressions") {
      it("creation") {

      }

      it("destruction") {

      }

      it("substitution") {

      }
    }


    describe("for numbers") {
      it("creation") {

      }

      it("destruction") {

      }

      it("attachment") {

      }

      it("detachment") {

      }
    }
  }

  describe("numbers") {
    it("can add numbers and then attach them to things") {

    }

    it("objects when you try to make an invalid attachment") {

    }
  }
}
