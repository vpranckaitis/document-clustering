package lt.vpranckaitis.document.clustering.dto

import spray.json.DefaultJsonProtocol

object ExtendedJsonProtocol extends DefaultJsonProtocol {
  implicit val configurationFormat = jsonFormat2(Configuration)
  implicit val EvaluationFormat = jsonFormat7(Evaluation)
}
