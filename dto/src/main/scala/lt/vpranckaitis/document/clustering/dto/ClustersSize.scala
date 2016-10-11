package lt.vpranckaitis.document.clustering.dto

case class ClustersSize(
    clusterCount: Int,
    standardDeviation: Double,
    medianAbsoluteDeviation: Double,
    smallestClusterSize: Int,
    largestClusterSize: Int)
