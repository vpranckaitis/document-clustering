package lt.vpranckaitis.document.clustering.clusterers.kmeans

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.initialization._
import de.lmu.ifi.dbs.elki.data.NumberVector
import de.lmu.ifi.dbs.elki.math.random.RandomFactory

sealed trait InitialMeans {
  def logMessage: String
  private [kmeans] def getAlgorithm() : AbstractKMeansInitialization[NumberVector]
}

object InitialMeans {
  case class Random(seed: Long) extends InitialMeans {
    override def logMessage: String = s"Random(seed = $seed)"

    override private[kmeans] def getAlgorithm() =
      new RandomlyChosenInitialMeans(RandomFactory.get(seed))
  }

  case class FarthestPoints(seed: Long) extends InitialMeans {
    override def logMessage: String = s"FarthestPoints(seed = $seed)"

    override private[kmeans] def getAlgorithm() =
      new FarthestPointsInitialMeans(RandomFactory.get(seed), true)
  }

  case class KMeansPlusPlus(seed: Long) extends InitialMeans {
    override def logMessage: String = s"KMeansPlusPlus(seed = $seed)"

    override private[kmeans] def getAlgorithm() =
      new KMeansPlusPlusInitialMeans(RandomFactory.get(seed))
  }
}
