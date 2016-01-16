package lt.vpranckaitis.documentclustering.storage

import org.joda.time.DateTime
import slick.driver.PostgresDriver.api._

import scala.concurrent.ExecutionContext.Implicits.global

class Storage {
  val db = Database.forConfig("document-clustering.database")



  def run = {
    import schema._

    for {
      insertAction <- db.run(articlesQuery += Article("delfi", "delfi2", "delfi", "delfi", new DateTime(), "delfi1", "delfi", "delfi")) recover { case _ => ()}
                                                                                                                                 selectAction <- db.run(articlesQuery.result)
    } yield selectAction

  }

}
