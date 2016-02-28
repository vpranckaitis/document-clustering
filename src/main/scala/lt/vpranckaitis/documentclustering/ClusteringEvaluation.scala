package lt.vpranckaitis.documentclustering

object ClusteringEvaluation {
  def purity(clusters: Seq[Seq[(String, Int)]]) = {
    val count = (clusters flatMap { _.unzip._2 }).sum
    val sumOfMaximums = (clusters map { _.unzip._2.max }).sum
    sumOfMaximums.toDouble / count
  }
}
