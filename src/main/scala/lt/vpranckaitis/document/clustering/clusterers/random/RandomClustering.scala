package lt.vpranckaitis.document.clustering.clusterers.random

import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

class RandomClustering(k: Int, seed: Int) extends Clusterer {
  override def logMessage: String = s"Random(k = $k, seed = $seed)"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val random = new scala.util.Random(seed)

    val grouped = documents map { (_, random.nextInt(k)) } groupBy { _._2 }

    val clusters = {
      grouped.values.toList map { cs => cs map { x => (x._1, 0.0) } }
    }

    ClusteringResults(clusters, logMessage)
  }
}
