package lt.vpranckaitis

import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import scala.util.Try

object Scraper extends App {

  val urls = List("http://www.delfi.lt/archive/?tod=01.01.2020&fromd=01.01.1999&channel=0&category=0")

  val articleUrls = urls flatMap { url =>
    ((new Browser().get(url)) >> elements(".arArticleT") >> attrs("href")("a")): Seq[String]
  }

  println(articleUrls)

  for (articleUrl <- articleUrls) {
    Try {
      val doc = new Browser().get(articleUrl)

      val article = doc.select("*[itemtype=http://schema.org/Article]")
      article.select("h1>span.headline-views").remove()
      article.select(".related-box, .image-article, .delfi-article-banner").remove()

      println(article >> text("*[itemprop=headline]"))
      println(article >> element("meta[itemprop=datePublished]") >> attr("content")("meta"))
      //println(article >> text("div[itemprop=articleBody]"))
    }
  }

}
