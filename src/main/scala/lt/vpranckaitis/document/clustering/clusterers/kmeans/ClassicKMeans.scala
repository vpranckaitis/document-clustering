package lt.vpranckaitis.document.clustering.clusterers.kmeans

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.{KMeans, KMeansMacQueen}
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.data.model.KMeansModel
import lt.vpranckaitis.document.clustering.SparseCosineDistanceFunction
import lt.vpranckaitis.document.clustering.clusterers.{Clusterer, DistanceFunction}
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class ClassicKMeans(k: Int, distanceFunction: DistanceFunction, initial: InitialMeans) extends Clusterer {

  val MaxIterations = 10000
  val logMessage = s"ClassicKMeans(k=$k, distanceFunction=${distanceFunction.logMessage}, initialMeans=${initial.logMessage})"

  private[kmeans] def getClusterer(): KMeans[SparseDoubleVector, KMeansModel] = {
    new KMeansMacQueen(distanceFunction.getAlgorithm(), k, MaxIterations, initial.getAlgorithm())
  }

  def clusterize(documents: Seq[Document]): ClusteringResults = {
    val clusterer = getClusterer()

    val database = buildDatabase(documents)

    val clusters = clusterer.run(database).getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      val mean = c.getModel.getMean
      cluster map { x => (x, SparseCosineDistanceFunction.STATIC.distance(mean, x)) } sortBy { _._2 }
    }
    ClusteringResults(clusters, logMessage)
  }
}
