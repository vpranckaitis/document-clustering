package lt.vpranckaitis.document.clustering.clusterers

import de.lmu.ifi.dbs.elki.data.`type`.VectorFieldTypeInformation
import de.lmu.ifi.dbs.elki.data.model.Model
import de.lmu.ifi.dbs.elki.data.{Cluster, NumberVector, SparseDoubleVector}
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.database.ids.{DBIDIter, DBIDRef}
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import de.lmu.ifi.dbs.elki.index.{Index, IndexFactory}
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

trait Clusterer {
  def logMessage: String
  def clusterize(documents: Seq[Document]): ClusteringResults

  private[clusterers] def buildDatabase(vectors: Seq[SparseDoubleVector], indexes: Array[IndexFactory[_ <: NumberVector, _ <: Index]] = Array()) = {
    var dimensionality = (vectors map { _.getDimensionality }).max

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, vectors.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, indexes.toList)
    database.initialize()
    database
  }

  private[clusterers] def extractClusters[T <: Model](database: StaticArrayDatabase)(c: Cluster[T]) = {
    def getDocument(it: DBIDRef) = database.getBundle(it).data(1).asInstanceOf[Document]
    def iterate(it: DBIDIter): Stream[Document] = if (it.valid) getDocument(it) #:: iterate(it.advance()) else Stream.empty

    iterate(c.getIDs.iter())
  }
}
