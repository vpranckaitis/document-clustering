package lt.vpranckaitis.document.clustering.clusterers.gdbscan

import de.lmu.ifi.dbs.elki.algorithm.clustering.gdbscan.{EpsilonNeighborPredicate, NeighborPredicate => ElkiNeighborPredicate}
import lt.vpranckaitis.document.clustering.clusterers.DistanceFunction

trait NeighborPredicate {
  def logMessage: String
  def getAlgorithm(): ElkiNeighborPredicate
}

object NeighborPredicate {
  case class Epsilon(eps: Double, distanceFunction: DistanceFunction) extends NeighborPredicate {
    override def logMessage: String = s"EpsilonNeighborPredicate(epsilon=$eps, distanceFunction=${distanceFunction.logMessage})"

    override def getAlgorithm(): ElkiNeighborPredicate = new EpsilonNeighborPredicate(eps, distanceFunction.getAlgorithm())
  }
}
