package lt.vpranckaitis.document.clustering.clusterers.hierarchical

import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.{AGNES, PointerDensityHierarchyRepresentationResult}
import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.extraction.{ExtractFlatClusteringFromHierarchy, HDBSCANHierarchyExtraction, SimplifiedHierarchyExtraction}
import de.lmu.ifi.dbs.elki.data.Clustering
import de.lmu.ifi.dbs.elki.data.model.DendrogramModel
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.clusterers.{Clusterer, DistanceFunction}
import lt.vpranckaitis.document.clustering.dto.Document

import scala.annotation.tailrec
import scala.collection.JavaConversions._

class Hierarchical(distanceFunction: DistanceFunction, linkageMethod: LinkageMethod, extractionMethod: ExtractionMethod) extends Clusterer {
  override def logMessage: String =
    s"Hierarchical(distanceFunction = ${distanceFunction.logMessage}, linkageMethod = ${linkageMethod.logMessage}, " +
      s"extractionMethod = ${extractionMethod.logMessage})"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val database = buildDatabase(documents)

    val clusteringAlgorithm = new AGNES(distanceFunction.getAlgorithm, linkageMethod.getAlgorithm())

    val results = extractionMethod.getAlgorithm(clusteringAlgorithm).run(database)

    val clusters = results.getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      cluster map { (_, 0.0) }
    }

    ClusteringResults(clusters, logMessage)
  }
}
