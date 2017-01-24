package lt.vpranckaitis.document.clustering.clusterers.optics

import de.lmu.ifi.dbs.elki.algorithm.clustering.optics.OPTICSXi
import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import de.lmu.ifi.dbs.elki.index.tree.spatial.rstarvariants.AbstractRTreeSettings
import de.lmu.ifi.dbs.elki.index.tree.spatial.rstarvariants.deliclu.DeLiCluTreeFactory
import de.lmu.ifi.dbs.elki.persistent.{MemoryPageFileFactory, Page}
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Document

import scala.collection.JavaConversions._

class OpticsXi(opticsAlgorithm: OpticsAlgorithm, xi: Double) extends Clusterer {
  override def logMessage: String = s"OpticsXi(opticsAlgorithm=${opticsAlgorithm.logMessage}, xi=$xi)"

  override def clusterize(documents: Seq[Document]): ClusteringResults = {
    val clusterer = new OPTICSXi(opticsAlgorithm.getAlgorithm(), xi)

    val deLiCluIndex = new DeLiCluTreeFactory[SparseDoubleVector](new MemoryPageFileFactory[Page](2000000000), new AbstractRTreeSettings())

    val database = buildDatabase(documents)

    val clusters = clusterer.run(database).getAllClusters map { c =>
      val cluster = extractClusters(database)(c)

      cluster map { x => (x, 0.0) }
    }
    ClusteringResults(clusters, logMessage)
  }
}
