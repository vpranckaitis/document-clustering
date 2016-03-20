package lt.vpranckaitis.document.clustering

object ClusteringEvaluation {
  case class PositivesAndNegatives(tp: Long, fp: Long, tn: Long, fn: Long)

  def purity(clusters: Seq[Seq[(String, Int)]]) = {
    val count = (clusters flatMap { _.unzip._2 }).sum
    val sumOfMaximums = (clusters map { _.unzip._2.max }).sum
    sumOfMaximums.toDouble / count
  }

  def positivesAndNegatives(clusters: Seq[Seq[(String, Int)]]) = {
    // http://nlp.stanford.edu/IR-book/html/htmledition/evaluation-of-clustering-1.html
    def chooseTwo(n: Long) = n * (n - 1) / 2

    val sizesOfCategoriesInClusters = clusters map { _.unzip._2 }
    val sizesOfClusters = sizesOfCategoriesInClusters map { _.sum }

    val sizesOfClustersInCategories = (clusters.flatten groupBy { _._1 } mapValues { _.unzip._2 }).unzip._2.toSeq
    val sizesOfCategories = sizesOfClustersInCategories map { _.sum }

    val numberOfElements = sizesOfClusters.sum
    val allPairs = chooseTwo(numberOfElements)

    val positives = (sizesOfClusters map { chooseTwo(_) }).sum
    val truePositives = sizesOfCategoriesInClusters.flatten.map(chooseTwo(_)).sum
    val falsePositives = positives - truePositives


    val negatives = allPairs - positives
    val falseNegatives = {
      val summands = for {
        categoryClustering <- sizesOfClustersInCategories
        categorySize = categoryClustering.sum.toLong
        categoryClusteringSize <- categoryClustering
      } yield (categorySize - categoryClusteringSize) * categoryClusteringSize

      summands.sum / 2
    }

    val trueNegatives = negatives - falseNegatives

    PositivesAndNegatives(truePositives, falsePositives, trueNegatives, falseNegatives)
  }

  def precission(clusters: Seq[Seq[(String, Int)]]): Double = precission(positivesAndNegatives(clusters))

  private def precission(pns: PositivesAndNegatives): Double = {
    import pns._
    tp.toDouble / (tp + fp)
  }

  def recall(clusters: Seq[Seq[(String, Int)]]): Double = recall(positivesAndNegatives(clusters))

  private def recall(pns: PositivesAndNegatives): Double = {
    import pns._
    tp.toDouble / (tp + fn)
  }

  def F1(clusters: Seq[Seq[(String, Int)]]): Double = {
    val pns = positivesAndNegatives(clusters)
    val p = precission(pns)
    val r = recall(pns)
    2 * p * r / (p + r)
  }
}
