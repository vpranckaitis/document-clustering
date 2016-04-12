package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Clustering extends App {

  val storage = new Storage

  val clustersF = storage.getArticlesByDataset(1) map { articles =>

    val wordBags = FeatureSelection.withStemming(articles)
    val dimensionality = wordBags.head.getDimensionality

    println(s"Article count: ${articles.size}, dimensionality: $dimensionality")

    val t = System.currentTimeMillis()
    val result = Clusterers.KMeans.random(wordBags)

    println((System.currentTimeMillis() - t) * 0.001)

    result
  }
  val clusters = Await.result(clustersF, Duration.Inf).toSeq

  val clusterCategories = clusters map { x => (x.unzip._1 groupBy { _.article.category } mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse) }
  clusterCategories foreach { c => println((c mkString "\n") + "\n---------") }

  println("##################")
  clusters foreach { ds => ds foreach { case (d, dist) => println(f"${dist}%1.4f ${d.article.category}: ${d.article.title}") }; println("------------") }

  println(s"Purity: ${ClusteringEvaluation.purity(clusterCategories)}")
  println(s"Precision: ${ClusteringEvaluation.precision(clusterCategories)}")
  println(s"Recall: ${ClusteringEvaluation.recall(clusterCategories)}")
  println(s"F1: ${ClusteringEvaluation.F1(clusterCategories)}")
}
