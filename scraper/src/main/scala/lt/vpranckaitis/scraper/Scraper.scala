package lt.vpranckaitis.scraper

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.documentclustering.storage.Storage
import net.ruippeixotog.scalascraper.browser.Browser
import org.jsoup.Connection

import scala.concurrent.Await
import scala.concurrent.duration._

object Scraper extends App with StrictLogging {

  val storage = new Storage()
  val browser = new Browser {
    override def requestSettings(conn: Connection) =
      conn
        .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6")
        .referrer("http://www.google.com")
        .header("Accept", "text/html,application/xhtml+xml,application/xml")
        .header("Accept-Charset", "utf-8")
        .timeout(15000)
  }

  val future = new Z15minScraper(storage, browser).scrape("http://www.15min.lt/ajax/articles/list?offset=2016-02-01 16:57:17")

  logger.info(Await.result(future, Duration.Inf).toString)
}
