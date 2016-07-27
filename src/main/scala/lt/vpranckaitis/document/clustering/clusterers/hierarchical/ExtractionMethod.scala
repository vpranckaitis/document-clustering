package lt.vpranckaitis.document.clustering.clusterers.hierarchical

import de.lmu.ifi.dbs.elki.algorithm.clustering.ClusteringAlgorithm
import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.extraction.ExtractFlatClusteringFromHierarchy
import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.{HierarchicalClusteringAlgorithm, LinkageMethod => ElkiLinkageMethod}
import de.lmu.ifi.dbs.elki.data.Clustering
import de.lmu.ifi.dbs.elki.data.model.DendrogramModel

sealed trait ExtractionMethod {
  def logMessage: String
  private [hierarchical] def getAlgorithm(clusteringAlgorithm: HierarchicalClusteringAlgorithm): ClusteringAlgorithm[Clustering[DendrogramModel]]
}

object ExtractionMethod {
  case class Flat(minK: Int) extends ExtractionMethod {
    override def logMessage: String = s"ExtractionMethod.Flat(minK = $minK)"
    override private[hierarchical] def getAlgorithm(clusteringAlgorithm: HierarchicalClusteringAlgorithm) = {
      new ExtractFlatClusteringFromHierarchy(clusteringAlgorithm, minK, false, true)
    }
  }
}