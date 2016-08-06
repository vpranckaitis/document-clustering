package lt.vpranckaitis.document.clustering.clusterers.hierarchical

import de.lmu.ifi.dbs.elki.algorithm.clustering.ClusteringAlgorithm
import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.{HierarchicalClusteringAlgorithm, PointerDensityHierarchyRepresentationResult, PointerHierarchyRepresentationResult}
import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.extraction.HDBSCANHierarchyExtraction
import de.lmu.ifi.dbs.elki.data.Clustering
import de.lmu.ifi.dbs.elki.data.`type`.TypeInformation
import de.lmu.ifi.dbs.elki.data.model.DendrogramModel
import de.lmu.ifi.dbs.elki.database.Database

import scala.annotation.tailrec

class HDBSCANHierarchyExtractionTargetK(algorithm: HierarchicalClusteringAlgorithm, targetK: Int, hierarchical: Boolean)
  extends ClusteringAlgorithm[Clustering[DendrogramModel]] {

  @tailrec
  private def binarySearch(min: Int,
                           max: Int,
                           hierarchy: PointerHierarchyRepresentationResult,
                           closest: Clustering[DendrogramModel]): Clustering[DendrogramModel] = {

    val mid = (min + max) / 2

    val extraction = extractClusters(hierarchy, mid)
    val clusterCount = extraction.getToplevelClusters.size

    val newClosest = if (Math.abs(clusterCount - targetK) < Math.abs(closest.getToplevelClusters.size - targetK))
      extraction
    else
      closest

    if (min == max)
      newClosest
    else if (clusterCount < targetK)
      binarySearch(min, mid, hierarchy, newClosest)
    else
      binarySearch(mid + 1, max, hierarchy, newClosest)
  }

  private def extractClusters(hierarchy: PointerHierarchyRepresentationResult, minClSize: Int) = {
    val coredist = hierarchy match {
      case x: PointerDensityHierarchyRepresentationResult =>
        x.getCoreDistanceStore
      case _ =>
        null
    }

    new HDBSCANHierarchyExtraction(algorithm, minClSize, hierarchical)
      .extractClusters(hierarchy.getDBIDs, hierarchy.getParentStore, hierarchy.getParentDistanceStore, coredist)
  }

  override def run(database: Database): Clustering[DendrogramModel] = {
    val hierarchy = algorithm.run(database)
    val initialResults = extractClusters(hierarchy, 1)

    binarySearch(1, hierarchy.getDBIDs.size, hierarchy, initialResults)
  }

  override def getInputTypeRestriction: Array[TypeInformation] = algorithm.getInputTypeRestriction
}
