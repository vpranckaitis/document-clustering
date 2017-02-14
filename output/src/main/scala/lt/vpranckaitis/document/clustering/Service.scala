package lt.vpranckaitis.document.clustering

import java.util.Locale

import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Experiment
import org.tartarus.snowball.ext.LithuanianStemmer
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Service(storage: Storage) {
  private val SplitPattern1 =  """[\s\.,!?\(\)\-–—„“":]+"""

  private def toExperimentSummary(e: Experiment) = {
    ExperimentSummary(
      e.id,
      e.datasetId,
      e.date,
      e.runtime,
      e.configuration.parseJson.convertTo[Configuration],
      e.evaluation.parseJson.convertTo[Evaluation],
      e.comment)
  }

  def getExperiments(): Future[Seq[ExperimentSummary]] = {
    storage.getExperimentsByDataset(5) map { experiments =>
      for {
        e <- experiments
      } yield toExperimentSummary(e)
    }
  }

  def getExperiment(id: Int): Future[Option[ExperimentSummary]] = {
    storage.getExperimentById(id) map { _ map { toExperimentSummary } }
  }

  def getClustersByExperimentId(experimentId: Int): Future[Map[Int, Cluster]] = {
    storage.getClustersByExperimentId(experimentId) map { clusters =>
      val grouped = clusters groupBy { _._1.cluster } mapValues { clusters =>
        for {
          article <- clusters.unzip._2
        } yield ArticleSummary(article.title, article.description, article.url)
      }

      grouped mapValues { Cluster(_) }
    }
  }

  def getClusterSizesByExperimentId(experimentId: Int): Future[Seq[Int]] = {
    storage.getClustersByExperimentId(experimentId) map { clusters =>
      val grouped = clusters groupBy { _._1.cluster } mapValues { _.size }

      grouped.values.toSeq.sorted(Ordering[Int].reverse)
    }
  }

  def getCommonWords(experimentId: Int, returnUnstemmed: Boolean = false): Future[Seq[ClusterInfo]] = {
    val stemmer = new LithuanianStemmer()
    def stemWord(s: String) = {
      stemmer.setCurrent(s)
      stemmer.stem()
      stemmer.getCurrent
    }

    def orderByOccurance(xs: Seq[String]) = {
      val counts = xs groupBy { identity } mapValues { _.size }
      counts.toSeq.sortBy(_._2)(Ordering[Int].reverse).unzip._1
    }

    def idf(tokenArrays: Seq[Seq[String]]): Map[String, Double] = {
      val n = tokenArrays.size
      val termAppearances = tokenArrays flatMap { _.distinct } groupBy { identity } mapValues { _.size }
      val idfs = termAppearances map { case (token, ni) => (token, Math.log(n.toDouble / ni)) }
      idfs
    }

    storage.getClustersByExperimentId(experimentId) map { clusters =>
      val grouped = clusters groupBy { _._1.cluster } mapValues { clusters =>
        for {
          article <- clusters.unzip._2
          words = article.text split SplitPattern1 map { _.toLowerCase(Locale.forLanguageTag("lt_LT")) }
          stemmed = words.toSeq withFilter { _.nonEmpty } map { w => (stemWord(w), w) }
        } yield stemmed
      }

      val unstemmedWords = grouped.values.flatten.flatten groupBy { _._1 } mapValues { xs =>
        orderByOccurance(xs.unzip._2.toSeq)
      }

      val onlyStems = grouped mapValues { _ map { _.unzip._1 } }

      val wholeIdf = idf(onlyStems.values.flatten.toSeq)

      val result = for {
        (cluster, tokenArrays) <- onlyStems
        clusterIdf = idf(tokenArrays)
        adjustedIdf = clusterIdf map { case (k, v) =>
          (k, wholeIdf(k) / (1 + v))
        }
        sorted = adjustedIdf.toSeq.sortBy(_._2)(Ordering[Double].reverse) take 10
      } yield (cluster, sorted.unzip._1)


      val unstemmedResult = if (returnUnstemmed) {
        result.toMap mapValues { _ flatMap { w => unstemmedWords(w) take 3 } }
      } else {
        result.toMap
      }

      val infos = unstemmedResult map { case (cluster, words) =>
        ClusterInfo(s"http://localhost:8000/experiments/$experimentId/clusters/$cluster", words)
      }

      infos.toSeq
    }
  }

  def getArticleIdClusterMap(experimentId: Int): Future[Seq[(Int, Int)]] = {
    storage.getClustersByExperimentId(experimentId) map { clusters =>
      clusters.unzip._1 map { x => (x.articleId, x.cluster) }
    }
  }
}
