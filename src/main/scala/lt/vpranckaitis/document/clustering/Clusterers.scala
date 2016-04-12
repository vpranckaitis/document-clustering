package lt.vpranckaitis.document.clustering

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.KMeansMacQueen
import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.initialization.RandomlyChosenInitialMeans
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.data.`type`.VectorFieldTypeInformation
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.database.ids.{DBIDIter, DBIDRef}
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import de.lmu.ifi.dbs.elki.math.random.RandomFactory

import scala.collection.JavaConversions._

object Clusterers {
  private def buildDatabase(vectors: Seq[SparseDoubleVector]) = {
    var dimensionality = (vectors map { _.getDimensionality }).max

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, vectors.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, null)
    database.initialize()
    database
  }

  object KMeans {
    def random(vectors: Seq[SparseDoubleVector]): Seq[Seq[(Document, Double)]] = {
      val clusterer = new KMeansMacQueen(SparseCosineDistanceFunction.STATIC, 10, 10000, new RandomlyChosenInitialMeans(RandomFactory.get(555.toLong)))

      val database = buildDatabase(vectors)

      clusterer.run(database).getToplevelClusters map { c =>
        def getDocument(it: DBIDRef) = database.getBundle(it).data(1).asInstanceOf[Document]
        def iterate(it: DBIDIter): Stream[Document] = if (it.valid) getDocument(it) #:: iterate(it.advance()) else Stream.empty

        val cluster = iterate(c.getIDs.iter()).toSeq

        val mean = c.getModel.getMean
        cluster map { x => (x, SparseCosineDistanceFunction.STATIC.distance(mean, x)) } sortBy { _._2 }
      }
    }
  }
}
