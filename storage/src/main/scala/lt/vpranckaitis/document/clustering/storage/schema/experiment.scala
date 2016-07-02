package lt.vpranckaitis.document.clustering.storage.schema

import com.github.tototoshi.slick.PostgresJodaSupport._
import org.joda.time.DateTime
import slick.driver.PostgresDriver.api._
import slick.lifted.{TableQuery, Tag}

object Experiment {
  def apply(datasetId: Int,
            date: DateTime,
            configuration: String,
            evaluation: String): Experiment =
    Experiment(None, datasetId, date, configuration, evaluation)
}

case class Experiment(id: Option[Int],
                      datasetId: Int,
                      date: DateTime,
                      configuration: String,
                      evaluation: String
                   ) extends Storable

class Experiments(tag: Tag) extends Table[Experiment](tag, "experiments") {
  def id = column[Int]("id", O.AutoInc, O.PrimaryKey)

  def datasetId = column[Int]("id_dataset")

  def date = column[DateTime]("date")

  def configuration = column[String]("configuration")

  def evaluation = column[String]("evaluation")

  def * = (id.?, datasetId, date, configuration, evaluation) <>
    ((Experiment.apply: (Option[Int], Int, DateTime, String, String) => Experiment).tupled, Experiment.unapply)
}

object experimentsQuery extends TableQuery(new Experiments(_)) {

}