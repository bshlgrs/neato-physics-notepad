package workspace
import scala.annotation._
import scala.collection.immutable


object Util {
  def traceShow[A](a: A): A = {
    println(a)
    a
  }
}
