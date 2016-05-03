package lt.vpranckaitis.document.clustering.clusterers.kmeans

import lt.vpranckaitis.document.clustering.Document

case class Results(clusters: Seq[Seq[(Document, Double)]], log: String)
