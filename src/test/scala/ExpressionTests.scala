import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import workspace._

class ExpressionTests extends FunSpec {
  describe("solving") {
    it("can solve an expression correctly") {
      val expr = Expression(0.5, Map("KE" -> -1, "m" -> 1, "v" -> 2))

      assert(expr.solve("KE") == Expression(0.5, Map("m" -> 1, "v" -> 2)))
      assert(expr.solve("v") == Expression(scala.math.pow(2, 0.5), Map("m" -> -0.5, "KE" -> 0.5)))
      assert(expr.solve("m") == Expression(2, Map("v" -> -2, "KE" -> 1)))
    }

    it("allows substitution") {
      // I have $$x = 3 * a^2 * b^2$$
      val expr = Expression(3, Map("a" -> 2, "b" -> 2))

      // I separately have $$a = 2 * c^2 * b$$
      val expr2 = Expression(2, Map("c" -> 2, "b" -> 1))

      // Combining these, I should get $$x = 3 * (2 * c^2 * b)^2 * b = 12 * c^4 * b^3
      assert(expr.sub("a", expr2) == Expression(12, Map("c" -> 4, "b" -> 4)))
    }

    it("lets you exponentiate expressions") {
      assert(Expression(4, Map("x" -> 3)).exponentiate(2) == Expression(16, Map("x" -> 6)))
    }

    it("lets you multiply expressions") {
      assert(Expression(2, Map("x" -> 2, "y" -> 1)) * Expression(2, Map("z" -> 2, "y" -> 3)) ==
        Expression(4, Map("x" -> 2, "y" -> 4, "z" -> 2))
      )
    }
  }
}
