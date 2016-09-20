package lt.vpranckaitis.scraper

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class DelfiScraper(storage: Storage, browser: Browser) extends StrictLogging {
  val archiveUrlTemplate = "http://www.delfi.lt/archive/?tod=01.01.2020&fromd=01.01.1999&channel=0&category=0&page="

  private def scrapeAndSave(url: String): Future[Int] = {
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
        println(article)
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


  def scrape(): Future[Int] = {
    val urls = for {
      page <- (1 to 9000).toStream
      archiveUrl = archiveUrlTemplate + page
      url <- ((browser.get(archiveUrl)) >> elements(".arArticleT") >> attrs("href")("a")): Seq[String]
    } yield url

    Future.fold(urls.map(scrapeAndSave))(0)(_ + _)
  }
}
