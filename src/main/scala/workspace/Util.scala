package workspace

object Util {
  def traceShow[A](a: A): A = {
    println(a)
    a
  }
}
