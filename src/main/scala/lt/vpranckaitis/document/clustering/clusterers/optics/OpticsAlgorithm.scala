package lt.vpranckaitis.document.clustering.clusterers.optics

import de.lmu.ifi.dbs.elki.algorithm.clustering.optics.OPTICSTypeAlgorithm

trait OpticsAlgorithm {
  def logMessage: String
  def getAlgorithm(): OPTICSTypeAlgorithm
}
