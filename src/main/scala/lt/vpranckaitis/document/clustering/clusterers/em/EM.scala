package lt.vpranckaitis.document.clustering.clusterers.em

import de.lmu.ifi.dbs.elki.algorithm.clustering.em.{DiagonalGaussianModelFactory, EM => ElkiEM}
import de.lmu.ifi.dbs.elki.algorithm.clustering.kmeans.initialization.RandomlyChosenInitialMeans
import de.lmu.ifi.dbs.elki.math.random.RandomFactory
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class EM(k: Int) extends Clusterer {
  val MaxIterations = 10000

  override def logMessage: String = ""

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val delta = 0.01
    val modelFactory = new DiagonalGaussianModelFactory(new RandomlyChosenInitialMeans(RandomFactory.get(1L)))
    val clusterer = new ElkiEM(k, delta, modelFactory, MaxIterations, false)

    val database = buildDatabase(documents)

    val result = clusterer.run(database)

    val clusters = result.getToplevelClusters map { c =>
      val cluster = extractClusters(database)(c)

      cluster map { (_, 0.0) }
    }

    ClusteringResults(clusters, logMessage)
  }
}
