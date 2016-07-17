package lt.vpranckaitis.document.clustering

import java.util.Locale

import lt.vpranckaitis.document.clustering.dto.Document
import lt.vpranckaitis.document.clustering.storage.schema.Article
import org.tartarus.snowball.ext.LithuanianStemmer

object FeatureSelection {
  private val SplitPattern1 =  """[\s\.,!?\(\)\-–—„“":]+"""

  class CompleteArticles private[FeatureSelection] (articles: Seq[Article], log: Vector[String]) {
    def takeText(): Raw = {
      val texts = articles map { _.text }
      new Raw(articles, texts, log :+ "takeText()")
    }
  }

  class Raw private[FeatureSelection] (articles: Seq[Article], texts: Seq[String], log: Vector[String]) {
    def split(): Tokenized = {
      val tokens = texts map { _ split SplitPattern1 filter { _.length > 0 } }
      new Tokenized(articles, tokens, log :+ s"split($SplitPattern1)")
    }
  }

  class Tokenized private[FeatureSelection] (articles: Seq[Article], tokenArrays: Seq[Array[String]], log: Vector[String]) {
    def stem(): Tokenized = {
      val lithuanianStemmer = new LithuanianStemmer()
      def stemWord(s: String) = {
        lithuanianStemmer.setCurrent(s)
        lithuanianStemmer.stem()
        lithuanianStemmer.getCurrent
      }
      val stemmed = tokenArrays map { _ map stemWord }
      new Tokenized(articles, stemmed, log :+ "stem()")
    }

    def toLowercase(): Tokenized = {
      val lowerCased = tokenArrays map { _ map { _.toLowerCase(Locale.forLanguageTag("lt_LT")) } }
      new Tokenized(articles, lowerCased, log :+ "toLowercase()")
    }

    def lengthAtLeast(from: Int): Tokenized  = {
      val filtered = tokenArrays map { _ filter { _.length >= from } }
      new Tokenized(articles, filtered, log :+ s"lengthAtLeast($from)")
    }

    def lengthAtMost(to: Int): Tokenized  = {
      val filtered = tokenArrays map { _ filter { _.length <= to } }
      new Tokenized(articles, filtered, log :+ s"lengthAtMost($to)")
    }

    def termFrequency(): Valued = {
      val termFrequencyArrays = tokenArrays map { tokens =>
        val tf = tokens groupBy { identity } mapValues { _.length.toDouble }
        tf.toArray
      }

      new Valued(articles, termFrequencyArrays, log :+ "termFrequency()")
    }

    def termFrequencyInverseDocumentFrequency(): Valued = {
      val n = articles.size
      val termAppearances = tokenArrays flatMap { _.distinct } groupBy { identity } mapValues { _.size }
      val idfs = termAppearances map { case (token, ni) => (token, Math.log(n.toDouble / ni)) }

      val termFrequencyArrays = tokenArrays map { tokens =>
        val tfs = tokens groupBy { identity } mapValues { _.length.toDouble }
        val tfIdfs = tfs map { case (token, tf) => (token, tf * idfs(token)) }

        tfIdfs.toArray
      }

      new Valued(articles, termFrequencyArrays, log :+ "termFrequencyInverseDocumentFrequency()")
    }
  }

  class Valued private[FeatureSelection] (articles: Seq[Article], featureArrays: Seq[Array[(String, Double)]], log: Vector[String]) {
    def valueAtLeast(x: Double) = {
      val filtered = featureArrays map { _ filter { _._2 >= x } }
      new Valued(articles, filtered, log :+ s"valueAtLeast($x)")
    }

    def valueAtMost(x: Double) = {
      val filtered = featureArrays map { _ filter { _._2 <= x } }
      new Valued(articles, filtered, log :+ s"valueAtMost($x)")
    }

    def leaveTermsWithHighestValues(percentile: Double) = {
      val filtered = featureArrays map { x =>
        if (x.length > 0) {
          val sortedValues = x.unzip._2.sorted(Ordering[Double].reverse)
          val thresholdValueIndex = Math.round(x.length * percentile).toInt
          val safeThresholdValueIndex = Math.max(Math.min(x.length - 1, thresholdValueIndex), 0)
          val thresholdValue = sortedValues(safeThresholdValueIndex)
          x filterNot { _._2 < thresholdValue }
        } else {
          x
        }
      }
      new Valued(articles, filtered, log :+ s"leaveTermsWithHighestValues($percentile)")
    }

    def leaveNHighestTerms(n: Int) = {
      val filtered = featureArrays map { x => x.sortBy(_._2)(Ordering[Double].reverse) take n }
      new Valued(articles, filtered, log :+ s"leaveNHighestTerms($n)")
    }

    def normalize() = {
      def normalizeArray(xs: Array[(String, Double)]) = {
        val squaredLength = (xs.unzip._2 map { x => x*x }).sum
        val length = Math.sqrt(squaredLength)

        xs map { case (k, v) => (k, v / length) }
      }

      val normalized = featureArrays map normalizeArray

      new Valued(articles, normalized, log :+ "normalize()")
    }

    def toFeatureVectors(): FeatureVectors = {
      val index = featureArrays.flatten.unzip._1.distinct.zipWithIndex.toMap

      val dimensionality = index.size + 1

      val featureVectors = featureArrays map { _ map { case (t, v) => (index(t), v) } }

      val documents = for {
        (fv, article) <- featureVectors zip articles
        sortedFv = fv sortBy { _._1 }
        (indices, values) = sortedFv.unzip
      } yield new Document(indices, values, dimensionality, article)

      FeatureVectors(documents, index, log :+ "toFeatureVector()")
    }
  }

  case class FeatureVectors(documents: Seq[Document], index: Map[String, Int], logMessage: Vector[String])

  def apply(articles: Seq[Article]) = {
    new CompleteArticles(articles, Vector.empty)
  }
}