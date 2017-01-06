package lt.vpranckaitis.document.clustering.clusterers.gdbscan

import de.lmu.ifi.dbs.elki.algorithm.clustering.gdbscan.GeneralizedDBSCAN
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class Gdbscan(neighborPred: NeighborPredicate, corePred: CorePredicate) extends Clusterer {
  override def logMessage: String = s"Gdbscan(neighborPredicate=${neighborPred.logMessage}, corePredicate=${corePred.logMessage})"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val clusterer = new GeneralizedDBSCAN(neighborPred.getAlgorithm(), corePred.getAlgorithm(), true)

    val database = buildDatabase(documents)

    val clusters = clusterer.run(database).getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      cluster map { x => (x, 0.0) }
    }
    ClusteringResults(clusters, logMessage)
  }
}
