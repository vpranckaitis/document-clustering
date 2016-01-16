package lt.vpranckaitis.scraper

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.documentclustering.storage.Storage
import lt.vpranckaitis.documentclustering.storage.schema.Article
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Scraper extends App with StrictLogging {

  val archiveUrlTemplate = "http://www.delfi.lt/archive/?tod=01.01.2020&fromd=01.01.1999&channel=0&category=0&page="

  val storage = new Storage()
  val browser = new Browser()

  def scrapeAndSave(url: String): Future[Int] = {
    val scrape = Try {
      val doc = browser.get(url)

      val articleHtml = doc.select("*[itemtype=http://schema.org/Article]")
      articleHtml.select("h1>span.headline-views").remove()
      articleHtml.select(".related-box, .image-article, .delfi-article-banner").remove()

      val urlParse = """http://([^/]+)/([^/]+)/([^/]+).*""".r

      val (source: String, category: String, subcategory: String) = url match {
        case urlParse(s, c, sc) => (s, c, sc)
      }

      val date: DateTime = DateTime.parse(articleHtml >> element("meta[itemprop=datePublished]") >> attr("content")("meta"))
      val title: String = articleHtml >> text("*[itemprop=headline]")
      val description: String = articleHtml >> text("*[itemprop=description]")
      val articleText: String = articleHtml >> text("div[itemprop=articleBody]")

      Article(source, url, category, subcategory, date, title, description, articleText)
    }
    scrape match {
      case Success(article) =>
        storage.save(article) recover {
          case ex =>
            logger.error("Failed saving", ex)
            0
        }
      case Failure(ex) =>
        logger.error("Failed scraping", ex)
        Future.successful(0)
    }
  }

  val urls = for {
    page <- (1 to 300).toStream
    archiveUrl = archiveUrlTemplate + page
    url <- ((browser.get(archiveUrl)) >> elements(".arArticleT") >> attrs("href")("a")): Seq[String]
  } yield url

  val foldedFutures = Future.fold(urls.map(scrapeAndSave))(0)(_ + _)

  logger.info(Await.result(foldedFutures, Duration.Inf).toString)
}
