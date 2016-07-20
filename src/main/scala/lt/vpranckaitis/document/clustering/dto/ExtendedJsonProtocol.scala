package lt.vpranckaitis.document.clustering.dto

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}

object ExtendedJsonProtocol extends DefaultJsonProtocol {
  implicit val configurationFormat = jsonFormat2(Configuration)
  implicit val EvaluationFormat = jsonFormat7(Evaluation)
  implicit val ExperimentSummaryFormat = jsonFormat6(ExperimentSummary)

  implicit object DateJsonFormat extends RootJsonFormat[DateTime] {

    private val parserISO : DateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis();

    override def write(obj: DateTime) = JsString(parserISO.print(obj))

    override def read(json: JsValue) : DateTime = json match {
      case JsString(s) => parserISO.parseDateTime(s)
      case _ => throw new DeserializationException("Error deserializing DateTime")
    }
  }
}
