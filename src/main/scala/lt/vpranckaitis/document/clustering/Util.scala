package lt.vpranckaitis.document.clustering

object Util {
  def time[T](f: => T) = {
    val startTime = System.currentTimeMillis()
    val result = f
    val duration = System.currentTimeMillis() - startTime
    (result, duration)
  }
}
