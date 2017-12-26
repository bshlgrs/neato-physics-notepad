package wrapper

import workspace._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("Gem.ToJS")
@JSExportAll
object ToJS {
  def convert(obj: Any): Any = obj match {
    case list: List[_] => js.Array(list.map(ToJS.convert) :_*)
    case set: Set[_] => js.Array(set.map(ToJS.convert).toSeq :_*)
    case map: Map[_, _] => throw new RuntimeException("don't use Maps")
    case (x, y) => js.Array(x, y)
    case (x, y, z) => js.Array(x, y, z)
    case x => x
  }
}
