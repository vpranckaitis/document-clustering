package lt.vpranckaitis.documentclustering.storage

import slick.driver.PostgresDriver.api._

import scala.concurrent.ExecutionContext.Implicits.global

class Storage {
  val db = Database.forConfig("document-clustering.database")



  def run = {
    val action = schema.articles += (0, "delfi", "delfi", "delfi", "delfi", "delfi", "delfi", "delfi")

    db.run(schema.articles.result) map {
      _ foreach println
    }
  }

}
