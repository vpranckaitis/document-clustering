package lt.vpranckaitis.documentclustering.storage

import slick.driver.PostgresDriver.api._
import slick.lifted.Tag

object schema {

  class Articles(tag: Tag) extends Table[(Int, String, String, String, String, String, String, String)](tag, "articles") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)

    def source = column[String]("source")

    def url = column[String]("url")

    def category = column[String]("category")

    def subcategory = column[String]("subcategory")

    def title = column[String]("title")

    def description = column[String]("description")

    def text = column[String]("text")

    def * = (id, source, url, category, subcategory, title, description, text)
  }

  val articles = TableQuery[Articles]

}
