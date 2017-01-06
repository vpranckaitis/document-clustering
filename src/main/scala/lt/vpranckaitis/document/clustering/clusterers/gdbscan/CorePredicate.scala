package lt.vpranckaitis.document.clustering.clusterers.gdbscan

import de.lmu.ifi.dbs.elki.algorithm.clustering.gdbscan.{MinPtsCorePredicate, CorePredicate => ElkiCorePredicate}

trait CorePredicate {
  def logMessage: String
  def getAlgorithm(): ElkiCorePredicate
}

object CorePredicate {
  case class MinPoints(n: Int) extends CorePredicate {
    override def logMessage: String = s"MinPointsCorePredicate(n=$n)"

    override def getAlgorithm(): ElkiCorePredicate = new MinPtsCorePredicate(n)
  }
}
