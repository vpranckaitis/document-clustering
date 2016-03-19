package lt.vpranckaitis.scraper

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.documentclustering.storage.Storage
import net.ruippeixotog.scalascraper.browser.Browser

import scala.concurrent.Await
import scala.concurrent.duration._

object Scraper extends App with StrictLogging {

  val storage = new Storage()
  val browser = new Browser()

  val future = new Z15minScraper(storage, browser).scrape("http://www.15min.lt/ajax/articles/list?offset=2016-03-15 12:32:41")

  logger.info(Await.result(future, Duration.Inf).toString)
}
