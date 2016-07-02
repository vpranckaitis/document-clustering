package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.clusterers.kmeans.{ClassicKMeans, DistanceFunction, InitialMeans}
import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Clustering extends App {

  val storage = new Storage

  val resultsF = storage.getArticlesByDataset(2) map { articles =>

    val featureVectors =
      FeatureSelection(articles).
        takeText().
        split().
        toLowercase().
        lengthAtLeast(3).
        stem().
        termFrequencyInverseDocumentFrequency().
        //leaveTermsWithHighestValues(0.25).
        leaveNHighestTerms(100).
        normalize().
        toFeatureVectors()

    val dimensionality = featureVectors.documents.head.getDimensionality

    println(s"Article count: ${articles.size}, dimensionality: $dimensionality")

    val t = System.currentTimeMillis()
    val clusterer = new ClassicKMeans(10, DistanceFunction.Cosine, InitialMeans.Random(999999))
    val result = clusterer.clusterize(featureVectors.documents)

    println((System.currentTimeMillis() - t) * 0.001)

    (result.clusters, featureVectors, clusterer)
  }
  val (clusters, featureVectors, clusterer) = Await.result(resultsF, Duration.Inf)

  val clusterCategories = clusters map { x => (x.unzip._1 groupBy { _.article.category } mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse) }
  clusterCategories foreach { c => println((c mkString "\n") + "\n---------") }

  println("##################")
  clusters foreach { ds => ds foreach { case (d, dist) => println(f"${dist}%1.4f ${d.article.category}: ${d.article.title}") }; println("------------") }

  println(featureVectors.log mkString ".")
  println(clusterer.logMessage)

  println(s"Purity: ${ClusteringEvaluation.purity(clusterCategories)}")
  println(s"Precision: ${ClusteringEvaluation.precision(clusterCategories)}")
  println(s"Recall: ${ClusteringEvaluation.recall(clusterCategories)}")
  println(s"F1: ${ClusteringEvaluation.F1(clusterCategories)}")
  println(s"Average entropy: ${ClusteringEvaluation.averageEntropy(clusterCategories)}")
  println(s"Clustering entropy: ${ClusteringEvaluation.entropy(clusterCategories)}")
  println(s"Weighted average cluster distance: ${ClusteringEvaluation.weightedAverageClusterDistance(clusters)}")
}
