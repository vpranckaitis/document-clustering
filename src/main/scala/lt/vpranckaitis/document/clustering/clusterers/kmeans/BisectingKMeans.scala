package lt.vpranckaitis.document.clustering.clusterers.kmeans

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.{KMeansBisecting, KMeansMacQueen}
import lt.vpranckaitis.document.clustering.SparseCosineDistanceFunction
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.dto.Document
import scala.collection.JavaConversions._

class BisectingKMeans(k: Int, innerKMeans: ClassicKMeans) extends Clusterer {
  val logMessage = s"BisectingKMeans(k=$k, innerKMeans=${innerKMeans.logMessage})"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val clusterer = new KMeansBisecting(k, innerKMeans.getClusterer())

    val database = buildDatabase(documents)

    val clusters = clusterer.run(database).getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      val mean = c.getModel.getMean
      cluster map { x => (x, SparseCosineDistanceFunction.STATIC.distance(mean, x)) } sortBy { _._2 }
    }
    ClusteringResults(clusters, logMessage)
  }
}
