package lt.vpranckaitis.document.clustering.clusterers.kmeans

import lt.vpranckaitis.document.clustering.dto.Document

case class ClusteringResults(clusters: Seq[Seq[(Document, Double)]], log: String)
