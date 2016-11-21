package lt.vpranckaitis.document.clustering

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.storage.schema.Article

object CategoryMapper extends StrictLogging {
  private val Auto = "auto"
  private val Lietuva = "lietuva"
  private val Pasaulis = "pasaulis"
  private val Kriminalai = "kriminalai"
  private val Gyvenimas = "gyvenimas"
  private val Sportas = "sportas"
  private val Mokslas = "mokslas"
  private val Nuomones = "nuomones"
  private val Pramogos = "pramogos"
  private val Verslas = "verslas"
  private val Kultura = "kultura"
  private val Kita = "kita"

  def apply(article: Article): String = {
    article.source match {
      case "www.delfi.lt" => delfi(article.category, article.subcategory)
      case "www.15min.lt" => `15min`(article.category, article.subcategory)
      case "www.alfa.lt" => alfa(article.category, article.subcategory)
    }
  }

  private val delfi = PartialFunction[(String, String), String] {
    case ("auto", _) => Auto
    case ("gyvenimas", _) => Gyvenimas
    case ("krepsinis", _) => Sportas
    case ("mokslas", _) => Mokslas
    case ("pilietis", _) => Nuomones
    case ("sportas", _) => Sportas
    case ("veidai", _) => Pramogos
    case ("verslas", _) => Verslas
    case ("news", "crime") => Kriminalai
    case ("news", "lithuania") => Lietuva
    case ("news", "world") => Pasaulis
    case ("news", "ringas") => Nuomones

    case ("grynas", "gyvenimas") => Gyvenimas
    case ("grynas", _) => Kita

    case ("5braskes", "grazios") => Gyvenimas
    case ("5braskes", "linksmos") => Pramogos
    case ("5braskes", "konkursai") => Kita
    case ("5braskes", _) => Gyvenimas

    case ("video", "auto") => Auto
    case ("video", "mokslas-ir-gamta") => Mokslas
    case ("video", "pramogos") => Pramogos
    case ("video", "sportas") => Sportas
    case ("video", "verslas") => Verslas
    case ("video", "aktualijos") => Kita
    case ("video", "transliacijos") => Kita
    case ("video", "sveikata-tv") => Kita
    case ("video", "stilius") => Kita
    case ("video", "laidos") => Kita

    case e => logger.warn("delfi " + e); Kita
  }

  private val `15min` = PartialFunction[(String, String), String] {
    case ("24sek", _) => Sportas
    case ("deuce", _) => Sportas
    case ("sportas", _) => Sportas
    case ("gazas", _) => Auto
    case ("ikrauk", _) => Kita
    case ("mokslasit", _) => Mokslas
    case ("verslas", _) => Verslas

    case ("naujiena", "lietuva") => Lietuva
    case ("naujiena", "pasaulis") => Pasaulis
    case ("naujiena", "nusikaltimaiirnelaimes") => Kriminalai
    case ("naujiena", "nuomones") => Nuomones
    case ("naujiena", "komentarai") => Nuomones
    case ("naujiena", "kultura") => Kultura
    case ("naujiena", "sveikata") => Gyvenimas

    case ("laima", "laimos-veidai") => Pramogos
    case ("laima", _) => Gyvenimas

    case ("pasaulis-kiseneje", _) => Kita

    case ("zmones", _) => Pramogos
    case ("ji24", _) => Gyvenimas
    case e => logger.warn("15min " + e); Kita
  }

  private val alfa = PartialFunction[(String, String), String] {
    case ("auto", _) => Auto
    case ("gyvenimas", _) => Gyvenimas
    case ("it", _) => Mokslas
    case ("kriminalai", _) => Kriminalai
    case ("kultūra", _) => Kultura
    case ("lietuva", _) => Lietuva
    case ("pasaulis", _) => Pasaulis
    case ("nuomonės ir komentarai", _) => Nuomones
    case ("pramogos", _) => Pramogos
    case ("sportas", _) => Sportas
    case ("verslas", _) => Verslas
    case ("laisvalaikis", _) => Pramogos
    case e => logger.warn("alfa " + e); Kita
  }
}
