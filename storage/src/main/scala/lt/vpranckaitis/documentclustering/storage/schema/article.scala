package lt.vpranckaitis.documentclustering.storage.schema

import com.github.tototoshi.slick.PostgresJodaSupport._
import org.joda.time.DateTime
import slick.driver.PostgresDriver.api._
import slick.lifted.{TableQuery, Tag}

object Article {
  def apply(source: String,
            url: String,
            category: String,
            subcategory: String,
            date: DateTime,
            title: String,
            description: String,
            text: String): Article =
    Article(None, source, url, category, subcategory, date, title, description, text)
}

case class Article(id: Option[Int],
                   source: String,
                   url: String,
                   category: String,
                   subcategory: String,
                   date: DateTime,
                   title: String,
                   description: String,
                   text: String) extends Storable

class Articles(tag: Tag) extends Table[Article](tag, "articles") {
  def id = column[Int]("id", O.AutoInc, O.PrimaryKey)

  def source = column[String]("source")

  def url = column[String]("url")

  def category = column[String]("category")

  def subcategory = column[String]("subcategory")

  def date = column[DateTime]("date")

  def title = column[String]("title")

  def description = column[String]("description")

  def text = column[String]("text")

  def * = (id.?, source, url, category, subcategory, date, title, description, text) <>
    ((Article.apply: (Option[Int], String, String, String, String, DateTime, String, String, String) => Article).tupled, Article.unapply)
}

object articlesQuery extends TableQuery(new Articles(_)) {

}