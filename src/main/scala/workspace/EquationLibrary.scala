package workspace
import cas.{Expression, RationalNumber}
import workspace.dimensions.SiDimension._
import workspace.dimensions._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.{Failure, Success, Try}

@JSExportTopLevel("Gem.EquationLibrary")
case class EquationLibrary(library: Map[Int, LibraryEquation]) {
  // unsafe
  @JSExport
  def getByEqId(id: Int): Equation = library(id)

  @JSExport
  def relevantEquationIds(searchTerm: String): js.Array[Int] = {
    if (searchTerm.filterNot(_.isWhitespace).isEmpty) {
      js.Array()
    } else {
      val searchTerms = searchTerm.toLowerCase.split(' ').toList
      val stuff = library.filter(pair => this.similarity(pair._2.tags, searchTerms)).take(5).keys
      js.Array(stuff.toList :_*)
    }
  }

  def similarity(tags: Set[String], searchTerms: List[String]): Boolean = {
    searchTerms.forall(term => tags.exists(_.contains(term)))
  }

  @JSExport
  def allAsJs: js.Dictionary[js.Object] = js.Dictionary(library.map({ case (i: Int, e: LibraryEquation) => i.toString -> e.toJsObject }).toSeq :_*)
}

@JSExportTopLevel("Gem.EquationLibraryOps")
object EquationLibrary {
  @JSExport
  def buildFromJs(equations: js.Array[LibraryEquationJs]): EquationLibrary = {
    EquationLibrary(equations.flatMap(eq => {
      Try(LibraryEquationJs.parse(eq)) match {
        case Success(leq: LibraryEquation) => List(leq.id -> leq)
        case Failure(x) => {
          Nil
        }
      }
    }).toMap)
  }
}
