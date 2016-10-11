package lt.vpranckaitis.document.clustering.dto

case class Evaluation(
    purity: Double,
    precision: Double,
    recall: Double,
    f1: Double,
    averageEntropy: Double,
    clusteringEntropy: Double,
    weightedAverageClusterDistance: Double,
    clustersSize: Option[ClustersSize])
