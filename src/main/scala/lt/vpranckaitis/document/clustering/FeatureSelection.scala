package lt.vpranckaitis.document.clustering

import java.util.Locale

import lt.vpranckaitis.document.clustering.storage.schema.Article
import org.tartarus.snowball.ext.LithuanianStemmer

object FeatureSelection {
  private val SplitPattern1 =  """[\s\.,!?\(\)\-–—„“":]+"""

  class Raw private[FeatureSelection] (articles: Seq[Article], log: Vector[String]) {
    def split(): Tokenized = {
      val tokens = articles map { _.text split SplitPattern1 filter { _.length > 0 } }
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
      val filtered = tokenArrays map { _ filter { _.size >= from } }
      new Tokenized(articles, filtered, log :+ s"lengthAtLeast($from)")
    }

    def lengthAtMost(to: Int): Tokenized  = {
      val filtered = tokenArrays map { _ filter { _.size <= to } }
      new Tokenized(articles, filtered, log :+ s"lengthAtMost($to)")
    }

    def termFrequency(): Valued = {
      val termFrequencyArrays = tokenArrays map { tokens =>
        val features = tokens groupBy { identity } mapValues { _.length.toDouble }
        features.toArray
      }

      new Valued(articles, termFrequencyArrays, log :+ "termFrequency()")
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

  case class FeatureVectors(documents: Seq[Document], index: Map[String, Int], log: Vector[String])

  def apply(articles: Seq[Article]) = {
    new Raw(articles, Vector.empty)
  }
}