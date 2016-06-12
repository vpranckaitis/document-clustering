package lt.vpranckaitis.scraper

import java.util.Locale

import com.sun.javaws.exceptions.InvalidArgumentException
import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class AlfaScraper(storage: Storage, browser: Browser) extends StrictLogging {
  val archiveUrlTemplate = "http://www.alfa.lt/feed/"

  val subcategoryMap = Map(
    "naujienos" -> "lietuva",
    "apžvalgos" -> "lietuva",
    "lietuva pasaulyje" -> "lietuva",
    "studijos" -> "lietuva",
    "politikų tribūna" -> "lietuva",
    "alfa reporteris" -> "lietuva",
    "aplinka" -> "lietuva",

    "nuomonės ir komentarai" -> "nuomonės ir komentarai",
    "archyvas" -> "nuomonės ir komentarai",
    "psichologo komentaras" -> "nuomonės ir komentarai",
    "dienos tema" -> "nuomonės ir komentarai",

    "europa" -> "pasaulis",
    "amerika" -> "pasaulis",
    "rusija" -> "pasaulis",
    "vidurio rytai ir afrika" -> "pasaulis",
    "pasaulio apžvalgos" -> "pasaulis",
    "azija ir okeanija" -> "pasaulis",
    "lotynų amerika" -> "pasaulis",
    "-pasaulis" -> "pasaulis",

    "avarijos" -> "kriminalai",
    "nusikaltimai" -> "kriminalai",
    "įvykiai" -> "kriminalai",
    "gaisrai" -> "kriminalai",
    "teismai" -> "kriminalai",

    "kultūrpolis" -> "kultūra",
    "knygos" -> "kultūra",
    "siluetai" -> "kultūra",
    "stambiu planu" -> "kultūra",
    "kalėdos 2013" -> "kultūra",

    "premjeros" -> "auto",
    "autonaujienos" -> "auto",
    "garažas" -> "auto",
    "eismas" -> "auto",
    "vairuotojų naujienos" -> "auto",

    "verslo naujienos" -> "verslas",
    "pasaulio verslas" -> "verslas",
    "energetika" -> "verslas",
    "transportas" -> "verslas",
    "nekilnojamasis turtas" -> "verslas",
    "žemės ūkis" -> "verslas",
    "verslo špionažas" -> "verslas",
    "vitrina" -> "verslas",
    "euro efektas" -> "verslas",
    "energetika paprastai" -> "verslas",
    "dėl mūsų krašto" -> "verslas",
    "statybos" -> "verslas",

    "krepšinis" -> "sportas",
    "futbolas" -> "sportas",
    "tenisas" -> "sportas",
    "kitas sportas" -> "sportas",
    "autosportas" -> "sportas",

    "it naujienos" -> "it",
    "mokslas" -> "it",
    "išradimai" -> "it",
    "ekologija" -> "it",
    "patarimai" -> "it",

    "žmonės" -> "pramogos",
    "elito kronikos" -> "pramogos",
    "raudonasis kilimas" -> "pramogos",
    "rusijos žvaigždės" -> "pramogos",
    "oskarai" -> "pramogos",
    "mados pasaulis" -> "pramogos",
    "grožio studija" -> "pramogos",
    "sunku patikėti" -> "pramogos",
    "interneto sensacija" -> "pramogos",
    "įdomūs faktai" -> "pramogos",
    "duetų naujienos" -> "pramogos",
    "dalyviai" -> "pramogos",
    "mylimiausi" -> "pramogos",
    "pasirodymai" -> "pramogos",
    "smagiai" -> "pramogos",
    "pramogų karuselė" -> "pramogos",
    "užsienio atlikėjai" -> "pramogos",


    "santykiai" -> "gyvenimas",
    "sveikata" -> "gyvenimas",
    "virtuvė" -> "gyvenimas",
    "namai" -> "gyvenimas",
    "gyvūnai" -> "gyvenimas",
    "horoskopai" -> "gyvenimas",
    "konkursai" -> "gyvenimas",
    "jaukiai" -> "gyvenimas",

    "pramogauk" -> "laisvalaikis",
    "tv" -> "laisvalaikis",
    "kelionės" -> "laisvalaikis",
    "muzika" -> "laisvalaikis",
    "kinas" -> "laisvalaikis",
    "eurovizija 2016" -> "laisvalaikis",
    "nauja legenda" -> "laisvalaikis"
  ) withDefaultValue ""

  private def scrapeAndSave(url: String): Future[Int] = {
    val scrape = Try {
      val doc = browser.get(url)

      val articleHtml = doc.select("*[itemtype=http://schema.org/NewsArticle]")
      articleHtml.select(".article__social_box").remove()
      articleHtml.select(".iaad").remove()
      articleHtml.select(".twitter-tweet").remove()

      val urlParse = """http://([^/]+).*""".r

      val source = url match {
        case urlParse(s) => s
      }

      val subcategory: String = (doc >> element("meta[property=article:section]") >> attr("content")("meta")).toLowerCase(Locale.forLanguageTag("lt_LT"))
      if (subcategory == "lnk go" || subcategory == "politologai")
        throw new InvalidArgumentException(Array(subcategory))

      val category = subcategoryMap(subcategory)

      if (category == "") logger.warn(s"""No category for "$subcategory" """)

      val date: DateTime = DateTime.parse(articleHtml >> element("span[itemprop=datePublished]") >> attr("content")("span"), DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
      val title: String = articleHtml >> text("*[itemprop=name]")
      val description: String = doc >> element("meta[name=description]") >> attr("content")("meta")
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
      case Failure(_: InvalidArgumentException) =>
        logger.warn(s"$url: lnk go")
        Future.successful(0)
      case Failure(ex) =>
        logger.error(s"Failed scraping $url", ex)
        Future.successful(0)
    }
  }


  def scrape(): Future[Int] = {
    val urls = for {
      page <- (2000 to 5000).toStream
      archiveUrl = archiveUrlTemplate + page
      url <- ((browser.get(archiveUrl)) >> elements("a") >> attrs("href")("a")): Seq[String]
    } yield url

    Future.fold(urls.map(scrapeAndSave))(0)(_ + _)
  }
}
