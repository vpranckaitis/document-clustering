package lt.vpranckaitis.document.clustering.clusterers

import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

trait Clusterer {
  def logMessage: String
  def clusterize(documents: Seq[Document]): ClusteringResults
}
