package lt.vpranckaitis.document.clustering.dto

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import spray.json._


object ExtendedJsonProtocol extends DefaultJsonProtocol {

  implicit object DateJsonFormat extends RootJsonFormat[DateTime] {

    private val parserISO : DateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis();

    override def write(obj: DateTime) = JsString(parserISO.print(obj))

    override def read(json: JsValue) : DateTime = json match {
      case JsString(s) => parserISO.parseDateTime(s)
      case _ => throw new DeserializationException("Error deserializing DateTime")
    }
  }

  implicit val configurationFormat = jsonFormat2(Configuration)
  implicit val clusterSizeFormat = jsonFormat5(ClustersSize)
  implicit val EvaluationFormat = jsonFormat8(Evaluation)
  implicit val ExperimentSummaryFormat = jsonFormat7(ExperimentSummary)
  implicit val ArticleSummaryFormat = jsonFormat3(ArticleSummary)
  implicit val ClusterFormat = jsonFormat1(Cluster)
  implicit val ClusterInfoFormat = jsonFormat3(ClusterInfo)
}
