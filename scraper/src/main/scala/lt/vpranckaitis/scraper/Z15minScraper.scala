package lt.vpranckaitis.scraper

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import org.joda.time.DateTime
import org.jsoup.Connection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class Z15minScraper(storage: Storage, var browser: Browser) extends StrictLogging {
  def reconnectBrowser(): Unit = {
    browser = new Browser {
      override def requestSettings(conn: Connection) =
        conn
          .userAgent(s"Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.${System.currentTimeMillis}")
          .referrer("http://www.google.com")
          .header("Accept", "text/html,application/xhtml+xml,application/xml")
          .header("Accept-Charset", "utf-8")
          .timeout(15000)
    }
  }

  private def scrapeAndSave(url: String): Future[Int] = {
    val scrape = Try {
      val doc = browser.get(url)

      val articleHtml = doc.select("*[itemtype=http://schema.org/Article]")
      articleHtml.select("h1>span.headline-views").remove()
      articleHtml.select(".related-box, .image-article, .delfi-article-banner").remove()

      val metas = articleHtml >> elements("*[itemprop]")
      val articleContent = metas.select("*[itemprop=articleBody]>.article_content")
      articleContent.select("div").remove()

      val urlParse = """http://([^/]+)/([^/]+)/([^/]+).*""".r

      val (source: String, category: String, subcategory: String) = url match {
        case urlParse(s, c, sc) => (s, c, sc)
      }

      val date: DateTime = DateTime.parse(metas >> element("meta[itemprop=datePublished]") >> attr("content")("meta"))
      val title: String = metas >> text("*[itemprop=headline]")
      val description: String = metas >> text("*[itemprop=description]")
      val articleText: String = articleContent >> text("*")

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
        logger.error(s"Failed scraping: $url", ex)
        Thread.sleep(5000)
        reconnectBrowser()
        scrapeAndSave(url)
    }
  }

  def scrape(url: String): Future[Int] = {
    def iterate(url: String): Stream[String] = {
      val document = Try(browser.get(url)) match {
        case Success(x) => x
        case _ =>
          logger.warn("Failed to get the article list")
          Thread.sleep(5000)
          reconnectBrowser()
          browser.get(url)
      }

      val articles = document.select(".wrapper")
      articles.select(".widget").remove()
      val articleUrls = articles >> elementList(".item-content h3 a") >> attr("href")("a")
      val nextOffset = "offset: '(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})'".r.findFirstMatchIn((document >> elements("script")).toString).fold("0"){x => x.group(1)}
      println(nextOffset)
      val nextUrl = s"http://www.15min.lt/ajax/articles/list?offset=$nextOffset"
      articleUrls.toStream #::: iterate(nextUrl)
    }

    val futures = iterate(url) take 300000 map scrapeAndSave
    Future.fold(futures)(0)(_ + _)
  }
}
