package lt.vpranckaitis.document.clustering.dto

import org.joda.time.DateTime

case class ExperimentSummary(
    datasetId: Int,
    date: DateTime,
    runtime: Long,
    configuration: Configuration,
    clusterCount: Option[Int],
    evaluation: Evaluation,
    comment: String)
