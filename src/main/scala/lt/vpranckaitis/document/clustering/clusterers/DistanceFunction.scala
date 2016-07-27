package lt.vpranckaitis.document.clustering.clusterers

import de.lmu.ifi.dbs.elki.distance.distancefunction.AbstractSpatialDistanceFunction
import lt.vpranckaitis.document.clustering.SparseCosineDistanceFunction

trait DistanceFunction {
  def logMessage: String
  def getAlgorithm(): AbstractSpatialDistanceFunction
}

object DistanceFunction {
  object Cosine extends DistanceFunction {
    override def logMessage: String = "CosineDistanceFunction"

    override def getAlgorithm(): AbstractSpatialDistanceFunction = SparseCosineDistanceFunction.STATIC
  }
}