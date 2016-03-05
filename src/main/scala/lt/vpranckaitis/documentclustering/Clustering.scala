package lt.vpranckaitis.documentclustering

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.KMeansMacQueen
import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.initialization.RandomlyChosenInitialMeans
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.data.`type`.VectorFieldTypeInformation
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.database.ids.{DBIDIter, DBIDRef}
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import de.lmu.ifi.dbs.elki.distance.distancefunction.CosineDistanceFunction
import de.lmu.ifi.dbs.elki.math.random.RandomFactory
import lt.vpranckaitis.documentclustering.storage.Storage

import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Clustering extends App {


  val clusterer = new KMeansMacQueen(SparseCosineDistanceFunction.STATIC, 10, 10000, new RandomlyChosenInitialMeans(RandomFactory.DEFAULT))

  val storage = new Storage
  val k = storage.streamArticles map { articles =>
    val tokens = articles map { a => (a.text split """[\s\.,!?\(\)\-„“":]+""" filter { _.length > 8 }, a)}
    val wordIndex = tokens.flatMap(_._1).distinct.zipWithIndex.toMap
    val reverseWordIndex = wordIndex map { case (key, value) => (value, key) }
    val dimensionality = wordIndex.values.max + 1

    val wordBags = tokens map { tokenArray =>
      val tokenCount = tokenArray._1 groupBy { x => x } map { case (k, v) => (wordIndex(k), v.size.toDouble) }
      val (indexes, values) = tokenCount.toSeq.sortBy(_._1).unzip
      new Document(indexes, values, dimensionality, tokenArray._2.category)
    }

    println(SparseCosineDistanceFunction.STATIC.distance(wordBags.head, wordBags.tail.head))
    println(CosineDistanceFunction.STATIC.distance(wordBags.head, wordBags.tail.head))

    println(dimensionality)

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, wordBags.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, null)
    database.initialize()
    println(database.getRelations)

    val t = System.currentTimeMillis()
    val result = clusterer.run(database).getToplevelClusters map { c =>
      def getCategory(it: DBIDRef) = database.getBundle(it).data(1).asInstanceOf[Document].category
      def iterate(it: DBIDIter): Stream[String] = if (it.valid) getCategory(it) #:: iterate(it.advance()) else Stream.empty

      (iterate(c.getIDs.iter()) groupBy { s => s } mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse)
    }

    println((System.currentTimeMillis() - t) * 0.001)

    result
  }
  val clusters = Await.result(k, Duration.Inf)
  clusters foreach { c => println((c mkString "\n") + "\n---------") }
  println(ClusteringEvaluation.purity(clusters))
}
