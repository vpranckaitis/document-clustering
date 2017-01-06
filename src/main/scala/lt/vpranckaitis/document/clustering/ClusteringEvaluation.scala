package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.dto.{ClustersSize, Document, Evaluation}

object ClusteringEvaluation {
  case class PositivesAndNegatives(tp: Long, fp: Long, tn: Long, fn: Long)

  def evaluate(clusters: Seq[Seq[(Document, Double)]]) = {
    val clusterCategories = clusters map { x =>
      (x.unzip._1 map { x => CategoryMapper(x.article) } groupBy identity mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse)
    }

    println(clusterCategories.size)
    println(clusterCategories)
    println(clusterCategories.flatten.groupBy(_._1).mapValues(_.unzip._2.sum).toList.sortBy(_._2)(Ordering[Int].reverse))

    val purity = ClusteringEvaluation.purity(clusterCategories)
    val precision = ClusteringEvaluation.precision(clusterCategories)
    val recall = ClusteringEvaluation.recall(clusterCategories)
    val f1 = ClusteringEvaluation.F1(clusterCategories)
    val averageEntropy = ClusteringEvaluation.averageEntropy(clusterCategories)
    val clusteringEntropy = ClusteringEvaluation.entropy(clusterCategories)
    val weightedAverageClusterDistance = ClusteringEvaluation.weightedAverageClusterDistance(clusters)
    val clustersSize = ClusteringEvaluation.clustersSize(clusters)

    Evaluation(
      purity = purity,
      precision = precision,
      recall = recall,
      f1 = f1,
      averageEntropy = averageEntropy,
      clusteringEntropy = clusteringEntropy,
      weightedAverageClusterDistance = weightedAverageClusterDistance,
      clustersSize = Some(clustersSize)
      )
  }

  def purity(clusters: Seq[Seq[(String, Int)]]) = {
    val count = (clusters flatMap { _.unzip._2 }).sum
    val maximums = clusters map { x =>
      val counts = x.unzip._2
      if (counts.nonEmpty)
        counts.max
      else
        0
    }
    val sumOfMaximums = maximums.sum
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

  def precision(clusters: Seq[Seq[(String, Int)]]): Double = precision(positivesAndNegatives(clusters))

  private def precision(pns: PositivesAndNegatives): Double = {
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
    val p = precision(pns)
    val r = recall(pns)
    2 * p * r / (p + r)
  }

  def entropy(clusters: Seq[Seq[(String, Int)]]): Double = {
    def log2(x: Double) = Math.log(x) / Math.log(2)

    val categoriesCount = clusters.flatten.distinct.size
    val elementCount = clusters.flatten.unzip._2.sum
    val addends = for {
      cluster <- clusters
      clusterSize = cluster.unzip._2.sum.toDouble
      clusterCategoryCount <- cluster.unzip._2
    } yield clusterCategoryCount * log2(clusterCategoryCount / clusterSize)

    -addends.sum / (elementCount * log2(categoriesCount))
  }

  def averageEntropy(clusters: Seq[Seq[(String, Int)]]): Double = {
    def log2(x: Double) = Math.log(x) / Math.log(2)

    def basicEntropy(xs: Seq[Int]) = {
      val n = xs.sum.toDouble
      -(xs map { x => (x / n) * log2(x / n) }).sum
    }

    val elementCount = clusters.flatten.unzip._2.sum

    val addends = for {
      cluster <- clusters
      clusterSize = cluster.unzip._2.sum.toDouble
      clusterCategoryCounts = cluster.unzip._2
    } yield basicEntropy(clusterCategoryCounts) * clusterSize / elementCount

    addends.sum
  }

  def weightedAverageClusterDistance(clusters: Seq[Seq[(Document, Double)]]): Double = {
    def average(xs: Seq[Double]) = xs.sum / xs.size
    val addends = for {
      c <- clusters
      averageDistance = average(c.unzip._2)
      weighted = averageDistance * c.size
    } yield weighted
    addends.sum / clusters.flatten.size
  }

  def clustersSize(clusters: Seq[Seq[(Document, Double)]]): ClustersSize = {
    val sizes = clusters map { _.size }
    val stDev = standardDeviation(sizes)
    val mad = medianAbsoluteDeviation(sizes)

    ClustersSize(
      standardDeviation = stDev,
      medianAbsoluteDeviation = mad,
      clusterCount = clusters.size,
      smallestClusterSize = sizes.min,
      largestClusterSize = sizes.max)
  }

  def standardDeviation(xs: Seq[Int]): Double = {
    val mean = xs.sum.toDouble / xs.size
    val squareDifferencesFromMean = xs map { _ - mean } map { x => x * x }
    Math.sqrt(squareDifferencesFromMean.sum / xs.size)
  }

  def medianAbsoluteDeviation(xs: Seq[Int]): Double = {
    val m = median(xs map { _.toDouble })
    median(xs map { x => Math.abs(x - m) })
  }

  def median(xs: Seq[Double]): Double = {
    val mid = xs.size / 2
    val sorted = xs.sorted
    if (xs.size % 2 == 1) {
      sorted(mid)
    } else {
      (sorted(mid) + sorted(mid - 1)) / 2
    }
  }

}
