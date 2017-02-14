package lt.vpranckaitis.document.clustering

import java.io.PrintWriter

import de.lmu.ifi.dbs.elki.data.{NumberVector, SparseDoubleVector}
import de.lmu.ifi.dbs.elki.data.`type`.{TypeUtil, VectorFieldTypeInformation}
import de.lmu.ifi.dbs.elki.database.StaticArrayDatabase
import de.lmu.ifi.dbs.elki.datasource.MultipleObjectsBundleDatabaseConnection
import de.lmu.ifi.dbs.elki.datasource.bundle.MultipleObjectsBundle
import de.lmu.ifi.dbs.elki.index.{Index, IndexFactory}
import de.lmu.ifi.dbs.elki.math.linearalgebra.pca.{FirstNEigenPairFilter, PCAFilteredRunner, StandardCovarianceMatrixBuilder}
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

object Util {
  def time[T](f: => T) = {
    val startTime = System.currentTimeMillis()
    val result = f
    val duration = System.currentTimeMillis() - startTime
    (result, duration)
  }

  def pca(documents: Seq[Document], n: Int) = {
    val covarMatrix = new StandardCovarianceMatrixBuilder()
    val pcaRunner = new PCAFilteredRunner(covarMatrix, new FirstNEigenPairFilter(n), 0, 0)
    val db = buildRelation(documents)
    val result = pcaRunner.processCovarMatrix(covarMatrix.processDatabase(db))
    val eigenVectors = result.getStrongEigenvectors
    for {
      d <- documents
      v = eigenVectors.transposeTimes(d.getColumnVector)
    } yield v
  }

  def writePcaToFile(documents: Seq[Document], n: Int, filename: String): Unit = {
    val pca = Util.pca(documents, 2)
    val pcaWithId = documents map { d => (d.article.id.get, CategoryMapper(d.article)) } zip pca
    val strings = (pcaWithId map { case ((id, category), v) => s"$id\t$category\t${v.get(0)}\t${v.get(1)}" })
    val pw = new PrintWriter(filename)
    pw.write("id\tcategory\tx\ty\n")
    pw.write(strings mkString "\n")
    pw.close()
  }

  private[this] def buildRelation(vectors: Seq[SparseDoubleVector], indexes: Array[IndexFactory[_ <: NumberVector, _ <: Index]] = Array()) = {
    var dimensionality = (vectors map { _.getDimensionality }).max

    val typeInformation = new VectorFieldTypeInformation(SparseDoubleVector.FACTORY, dimensionality)
    val multipleObjectsBundle = MultipleObjectsBundle.makeSimple(typeInformation, vectors.toList)
    val databaseConnection = new MultipleObjectsBundleDatabaseConnection(multipleObjectsBundle)
    val database = new StaticArrayDatabase(databaseConnection, indexes.toList)
    database.initialize()
    database.getRelation(typeInformation)
  }
}
