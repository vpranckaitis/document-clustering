package lt.vpranckaitis.document.clustering.clusterers.optics

import de.lmu.ifi.dbs.elki.algorithm.clustering.optics.{DeLiClu => ElkiDeLiClu}
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.clusterers.{Clusterer, DistanceFunction}
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class DeLiClu(distanceFunction: DistanceFunction, minPts: Int) extends OpticsAlgorithm {
  override def logMessage: String = s"DeLiClu(distanceFunction=${distanceFunction.logMessage}, minPts=$minPts)"

  override def getAlgorithm() = new ElkiDeLiClu(distanceFunction.getAlgorithm(), minPts)
}
