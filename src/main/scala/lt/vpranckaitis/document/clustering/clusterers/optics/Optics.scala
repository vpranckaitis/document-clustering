package lt.vpranckaitis.document.clustering.clusterers.optics

import de.lmu.ifi.dbs.elki.algorithm.clustering.optics.{AbstractOPTICS, GeneralizedOPTICS, OPTICSHeap, DeLiClu => ElkiDeLiClu}
import lt.vpranckaitis.document.clustering.clusterers.DistanceFunction

class Optics(distanceFunction: DistanceFunction, eps: Double, minPts: Int) extends OpticsAlgorithm {
  override def logMessage: String = s"(distanceFunction=${distanceFunction.logMessage}, epsilon=$eps, minPts=$minPts)"

  override def getAlgorithm() = new OPTICSHeap(distanceFunction.getAlgorithm(), eps, minPts)
}
