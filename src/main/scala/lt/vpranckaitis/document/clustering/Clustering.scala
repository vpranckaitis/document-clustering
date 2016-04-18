package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Clustering extends App {


  println(s"Entropy: ${ClusteringEvaluation.entropy(Seq(Seq(("a",1)), Seq(("a",1)), Seq(("a",1))))}")

  val storage = new Storage

  val resultsF = storage.getArticlesByDataset(1) map { articles =>

    val featureVectors =
      FeatureSelection(articles).
        split().
        lengthAtLeast(3).
        stem().
        termFrequency().
        normalize().
        toFeatureVectors()

    val dimensionality = featureVectors.documents.head.getDimensionality

    println(s"Article count: ${articles.size}, dimensionality: $dimensionality")

    val t = System.currentTimeMillis()
    val result = Clusterers.KMeans.random(featureVectors.documents)

    println((System.currentTimeMillis() - t) * 0.001)

    (result.toSeq, featureVectors)
  }
  val (clusters, featureVectors) = Await.result(resultsF, Duration.Inf)

  val clusterCategories = clusters map { x => (x.unzip._1 groupBy { _.article.category } mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse) }
  clusterCategories foreach { c => println((c mkString "\n") + "\n---------") }

  println("##################")
  clusters foreach { ds => ds foreach { case (d, dist) => println(f"${dist}%1.4f ${d.article.category}: ${d.article.title}") }; println("------------") }

  println(featureVectors.log mkString ".")

  println(s"Purity: ${ClusteringEvaluation.purity(clusterCategories)}")
  println(s"Precision: ${ClusteringEvaluation.precision(clusterCategories)}")
  println(s"Recall: ${ClusteringEvaluation.recall(clusterCategories)}")
  println(s"F1: ${ClusteringEvaluation.F1(clusterCategories)}")
  println(s"Average entropy: ${ClusteringEvaluation.averageEntropy(clusterCategories)}")
  println(s"Clustering entropy: ${ClusteringEvaluation.entropy(clusterCategories)}")
}
