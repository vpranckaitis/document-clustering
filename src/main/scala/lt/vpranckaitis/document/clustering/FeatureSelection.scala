package lt.vpranckaitis.document.clustering

import java.util.Locale

import lt.vpranckaitis.document.clustering.storage.schema.Article
import org.tartarus.snowball.ext.LithuanianStemmer

object FeatureSelection {
  def withStemming(articles: Seq[Article]) = {
    val lithuanianStemmer = new LithuanianStemmer()
    def stem(s: String) = {
      lithuanianStemmer.setCurrent(s)
      lithuanianStemmer.stem()
      lithuanianStemmer.getCurrent
    }

    def splitAndStem(text: String) = {
      val splitted = text split """[\s\.,!?\(\)\-„“":]+""" filter { _.length > 0 }
      splitted map { w => stem(w.toLowerCase(Locale.forLanguageTag("lt_LT"))) }
    }

    val tokens = articles map { a => splitAndStem(a.text) } zip articles
    val wordIndex = tokens.flatMap(_._1).distinct.zipWithIndex.toMap
    val reverseWordIndex = wordIndex map { case (key, value) => (value, key) }
    val dimensionality = wordIndex.values.max + 1

    val wordBags = tokens map { tokenArray =>
      val tokenCount = tokenArray._1 groupBy { x => x } map { case (k, v) => (wordIndex(k), v.size.toDouble) }
      val vectorLength = Math.sqrt((tokenCount.values map { x => x*x }).sum)
      val (indexes, values) = (tokenCount mapValues { _ / vectorLength }).toSeq.sortBy(_._1).unzip
      new Document(indexes, values, dimensionality, tokenArray._2)
    }

    wordBags
  }
}
