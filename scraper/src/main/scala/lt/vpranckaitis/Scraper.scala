package lt.vpranckaitis

import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object Scraper extends App {
  val doc = new Browser().get("http://www.delfi.lt/video/laidos/1000-receptu-tv/kova-su-maisto-svaistymu-pasiimkite-namo-restoranuose-nesuvalgyta-maista.d?id=70064130")

  val article = doc.select("*[itemtype=http://schema.org/Article]")
  article.select("h1>span.headline-views").remove()

  println(article >> text("*[itemprop=headline]"))
  println(article >> element("meta[itemprop=datePublished]") >> attr("content")("meta"))
  println(article >> text("div[itemprop=articleBody]"))
}
