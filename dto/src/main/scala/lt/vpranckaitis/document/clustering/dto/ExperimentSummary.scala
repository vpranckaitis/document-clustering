package lt.vpranckaitis.document.clustering.dto

import org.joda.time.DateTime

case class ExperimentSummary(
    id: Option[Int],
    datasetId: Int,
    date: DateTime,
    runtime: Long,
    configuration: Configuration,
    evaluation: Evaluation,
    comment: String)
