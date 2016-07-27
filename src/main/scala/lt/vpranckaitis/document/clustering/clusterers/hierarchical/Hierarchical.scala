package lt.vpranckaitis.document.clustering.clusterers.hierarchical

import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.AGNES
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.clusterers.{Clusterer, DistanceFunction}
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class Hierarchical(distanceFunction: DistanceFunction, linkageMethod: LinkageMethod, extractionMethod: ExtractionMethod) extends Clusterer {
  override def logMessage: String =
    s"Hierarchical(distanceFunction = ${distanceFunction.logMessage}, linkageMethod = ${linkageMethod.logMessage}, " +
      s"extractionMethod = ${extractionMethod.logMessage})"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val database = buildDatabase(documents)

    val clusteringAlgorithm = new AGNES(distanceFunction.getAlgorithm, linkageMethod.getAlgorithm())

    val result = extractionMethod.getAlgorithm(clusteringAlgorithm).run(database)

    val clusters = result.getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      cluster map { (_, 0.0) }
    }

    ClusteringResults(clusters, logMessage)
  }
}
