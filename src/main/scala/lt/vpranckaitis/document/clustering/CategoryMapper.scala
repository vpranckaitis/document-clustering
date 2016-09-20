package lt.vpranckaitis.document.clustering

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.storage.schema.Article

object CategoryMapper extends StrictLogging {
  private val Auto = "auto"
  private val Naujienos = "naujienos"
  private val Gyvenimas = "gyvenimas"
  private val Sportas = "sportas"
  private val Mokslas = "mokslas"
  private val Nuomones = "nuomones"
  private val Pramogos = "pramogos"
  private val Verslas = "verslas"
  private val Kultura = "kultura"

  def apply(article: Article): String = {
    article match {
      case Article(_, "www.delfi.lt", _, "video", subcategory, _, _, _, _) =>
        delfiVideo(subcategory)
      case _ =>
        val mapperBySource = sources(article.source)
        mapperBySource(article.category)
    }
  }

  private def default(s: String): String = {
    logger.warn(s"Unrecognized category $s")
    s
  }

  private val delfi = Map(
    "auto" -> Auto,
    "es" -> Naujienos,
    "gyvenimas" -> Gyvenimas,
    "krepsinis" -> Sportas,
    "lmz" -> Gyvenimas,
    "mokslas" -> Mokslas,
    "news" -> Naujienos,
    "pilietis" -> Nuomones,
    "projektai" -> Gyvenimas,
    "sportas" -> Sportas,
    "vasara" -> Naujienos,
    "veidai" -> Pramogos,
    "verslas" -> Verslas
  ) withDefault default

  private val `15min` = Map(
    "24sek" -> Sportas,
    "deuce" -> Sportas,
    "gazas" -> Auto,
    "ikrauk" -> Nuomones,
    "ji24" -> Gyvenimas,
    "laima" -> Gyvenimas,
    "mokslasit" -> Mokslas,
    "naujiena" -> Naujienos,
    "pasaulis-kiseneje" -> Kultura,
    "sportas" -> Sportas,
    "tavoroma" -> Kultura,
    "verslas" -> Verslas,
    "zmones" -> Pramogos
  ) withDefault default

  private val alfa = Map(
    "auto" -> Auto,
    "gyvenimas" -> Gyvenimas,
    "it" -> Mokslas,
    "kriminalai" -> Naujienos,
    "kultūra" -> Kultura,
    "laisvalaikis" -> Pramogos,
    "lietuva" -> Naujienos,
    "nuomonės ir komentarai" ->	Nuomones,
    "pasaulis" -> Naujienos,
    "pramogos" -> Pramogos,
    "sportas" -> Sportas,
    "verslas" -> Verslas
  ) withDefault default

  private val `5braskes` = Map(
    "grazios" -> Gyvenimas,
    "karstos" -> Gyvenimas,
    "konkursai" -> Pramogos,
    "linksmos" -> Pramogos,
    "skanios" -> Gyvenimas,
    "sveikos" -> Gyvenimas
  ) withDefault default

  private val grynas = Map(
    "aplinka" -> Gyvenimas,
    "gamta" -> Gyvenimas,
    "gyvenimas" -> Gyvenimas,
    "tv" -> Gyvenimas
  ) withDefault default

  private val delfiVideo = Map(
    "aktualijos" -> Naujienos,
    "auto" -> Auto,
    "laidos" -> Pramogos,
    "mokslas-ir-gamta" -> Mokslas,
    "pramogos" -> Pramogos,
    "sportas" -> Sportas,
    "stilius" -> Gyvenimas,
    "sveikata-tv" -> Gyvenimas,
    "transliacijos" -> Gyvenimas,
    "verslas" -> Verslas
  ) withDefault default

  private val sources = Map(
    "www.15min.lt" -> `15min`,
    "www.alfa.lt" -> alfa,
    "www.delfi.lt" -> delfi,
    "grynas.delfi.lt" -> grynas,
    "www.5braskes.lt" -> `5braskes`
  ) withDefaultValue { Map.empty[String, String] withDefault default }
}
