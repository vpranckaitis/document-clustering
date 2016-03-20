package lt.vpranckaitis.document.clustering

import java.util.Locale

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.KMeansMacQueen
import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.initialization.RandomlyChosenInitialMeans
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.data.`type`.VectorFieldTypeInformation
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.database.ids.{DBIDIter, DBIDRef}
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import de.lmu.ifi.dbs.elki.math.random.RandomFactory
import lt.vpranckaitis.document.clustering.storage.Storage
import org.tartarus.snowball.ext.LithuanianStemmer

import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Clustering extends App {


  val clusterer = new KMeansMacQueen(SparseCosineDistanceFunction.STATIC, 10, 10000, new RandomlyChosenInitialMeans(RandomFactory.DEFAULT))

  val storage = new Storage

  val clustersF = storage.getArticlesByDataset(1) map { articles =>

    val lithuanianStemmer = new LithuanianStemmer()
    def stem(s: String) = {
      lithuanianStemmer.setCurrent(s)
      lithuanianStemmer.stem()
      lithuanianStemmer.getCurrent
    }

    def splitAndStem(text: String) = {
      val splitted = text split """[\s\.,!?\(\)\-„“":]+""" filter { _.length > 0 }
      splitted map { w => stem(w.toLowerCase(Locale.forLanguageTag("lt_LT"))) }
    }

    val tokens = articles map { a => (splitAndStem(a.text), a)}
    val wordIndex = tokens.flatMap(_._1).distinct.zipWithIndex.toMap
    val reverseWordIndex = wordIndex map { case (key, value) => (value, key) }
    val dimensionality = wordIndex.values.max + 1

    val wordBags = tokens map { tokenArray =>
      val tokenCount = tokenArray._1 groupBy { x => x } map { case (k, v) => (wordIndex(k), v.size.toDouble) }
      val (indexes, values) = tokenCount.toSeq.sortBy(_._1).unzip
      new Document(indexes, values, dimensionality, tokenArray._2)
    }

    println(s"Article count: ${articles.size}, dimensionality: $dimensionality")

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, wordBags.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, null)
    database.initialize()

    val t = System.currentTimeMillis()
    val result = clusterer.run(database).getToplevelClusters map { c =>
      def getDocument(it: DBIDRef) = database.getBundle(it).data(1).asInstanceOf[Document]
      def iterate(it: DBIDIter): Stream[Document] = if (it.valid) getDocument(it) #:: iterate(it.advance()) else Stream.empty

      val cluster = iterate(c.getIDs.iter()).toSeq

      val mean = c.getModel.getMean
      cluster map { x => (x, SparseCosineDistanceFunction.STATIC.distance(mean, x)) } sortBy { _._2 }
    }

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
