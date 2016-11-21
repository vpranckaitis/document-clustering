package lt.vpranckaitis.document.clustering.clusterers.hierarchical

import de.lmu.ifi.dbs.elki.algorithm.clustering.hierarchical.{CompleteLinkageMethod, GroupAverageLinkageMethod, SingleLinkageMethod, LinkageMethod => ElkiLinkageMethod}

sealed trait LinkageMethod {
  def logMessage: String
  private [hierarchical] def getAlgorithm() : ElkiLinkageMethod
}

object LinkageMethod {
  case object Single extends LinkageMethod {
    override def logMessage: String = "LinkageMethod.Single"
    override private[hierarchical] def getAlgorithm(): ElkiLinkageMethod = SingleLinkageMethod.STATIC
  }

  case object GroupAverage extends LinkageMethod {
    override def logMessage: String = "LinkageMethod.GroupAverage"
    override private[hierarchical] def getAlgorithm(): ElkiLinkageMethod = GroupAverageLinkageMethod.STATIC
  }

  case object Complete extends LinkageMethod {
    override def logMessage: String = "LinkageMethod.Complete"
    override private[hierarchical] def getAlgorithm(): ElkiLinkageMethod = CompleteLinkageMethod.STATIC
  }
}