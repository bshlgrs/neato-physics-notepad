package wrapper

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("GemUtils")
@JSExportAll
object GemUtils {
  type Point = (Double, Double)
  def minimumSpanningTree(positions: js.Array[js.Array[Double]]): js.Array[js.Array[Double]] = {

    val positionsList = positions.map((x) => x(0) -> x(1)).toSet

    def euclideanDistance(a: Point, b: Point): Double = math.sqrt(math.pow(a._1 - b._1, 2) + math.pow(a._2 - b._2, 2))

    def mst(positions: Set[Point]): Set[(Point, Point)] = {

      def mstLoop(selectedNodes: Set[Point], selectedEdges: Set[(Point, Point)]): Set[(Point, Point)] = {
        if (selectedNodes == positions)
          selectedEdges
        else {
          val newEdge = (for {
            fromNode <- selectedNodes
            toNode <- positions -- selectedNodes
          } yield (fromNode, toNode)).minBy({case (a, b) => euclideanDistance(a, b) })

          mstLoop(selectedNodes + newEdge._2, selectedEdges + newEdge)
        }
      }

      if(positions.isEmpty)
        Set()
      else
        mstLoop(Set(positions.head), Set())
    }

    js.Array(mst(positionsList).toList.map({case ((x1, y1), (x2, y2)) => js.Array(x1, y1, x2, y2)}) :_*)
  }
}
