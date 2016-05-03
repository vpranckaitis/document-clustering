package lt.vpranckaitis.document.clustering.clusterers.kmeans

import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.KMeansMacQueen
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.data.`type`.VectorFieldTypeInformation
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.database.ids.{DBIDIter, DBIDRef}
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import lt.vpranckaitis.document.clustering.{Document, SparseCosineDistanceFunction}

import scala.collection.JavaConversions._

class ClassicKMeans(k: Int, distanceFunction: DistanceFunction, initial: InitialMeans) {

  val MaxIterations = 10000
  val logMessage = s"ClassicKMeans(k=$k, distanceFunction=${distanceFunction.logMessage}, initialMeans=${initial.logMessage})"

  private def buildDatabase(vectors: Seq[SparseDoubleVector]) = {
    var dimensionality = (vectors map { _.getDimensionality }).max

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, vectors.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, null)
    database.initialize()
    database
  }

  def clusterize(documents: Seq[Document]): Results = {
    val clusterer = new KMeansMacQueen(distanceFunction.getAlgorithm(), k, MaxIterations, initial.getAlgorithm())

    val database = buildDatabase(documents)

    val clusters = clusterer.run(database).getToplevelClusters map { c =>
      def getDocument(it: DBIDRef) = database.getBundle(it).data(1).asInstanceOf[Document]
      def iterate(it: DBIDIter): Stream[Document] = if (it.valid) getDocument(it) #:: iterate(it.advance()) else Stream.empty

      val cluster = iterate(c.getIDs.iter())

      val mean = c.getModel.getMean
      cluster map { x => (x, SparseCosineDistanceFunction.STATIC.distance(mean, x)) } sortBy { _._2 }
    }

    Results(clusters, logMessage)
  }
}
