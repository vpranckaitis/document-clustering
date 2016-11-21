package lt.vpranckaitis.document.clustering

import java.io.BufferedReader

import lt.vpranckaitis.document.clustering.ExperimentService.Experiment
import lt.vpranckaitis.document.clustering.clusterers.hierarchical.{ExtractionMethod, Hierarchical, LinkageMethod}
import lt.vpranckaitis.document.clustering.clusterers.{DistanceFunction, random}
import lt.vpranckaitis.document.clustering.clusterers.kmeans.{BisectingKMeans, ClassicKMeans, InitialMeans}
import lt.vpranckaitis.document.clustering.clusterers.random.RandomClustering
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.{Source, StdIn}

object Clustering extends App {

  println("Enter Y to continue:")
  if (StdIn.readLine().toLowerCase != "y") {
    sys.exit()
  }

  val experimentService = new ExperimentService(new Storage)

  val experiments: Seq[Experiment] = for {
    k <- 9 to 13
    initialMeansAlgorithm <- Seq(InitialMeans.Random, InitialMeans.FarthestPoints, InitialMeans.KMeansPlusPlus)
    seed <- Seq(45, 72468, 92438, 242, 85436)
    initialMeans = initialMeansAlgorithm(seed)
    //linkage <- Seq(LinkageMethod.Complete, LinkageMethod.GroupAverage, LinkageMethod.Single)
  } yield { articles: Seq[Article] =>
    val featureVectors =
      FeatureSelection(articles).
        takeText().
        split().
        toLowercase().
        lengthAtLeast(1).
        stem().
        //termFrequency().
        termFrequencyInverseDocumentFrequency().
        normalize().
        toFeatureVectors()

    val clusterer = new BisectingKMeans(k, new ClassicKMeans(2, DistanceFunction.Cosine, initialMeans))
    //val clusterer = new Hierarchical(DistanceFunction.Cosine, linkage, ExtractionMethod.HDBSCAN(k))
    //val clusterer = new ClassicKMeans(k, DistanceFunction.Cosine, initialMeans)
    //val clusterer = new RandomClustering(k, seed)

    (featureVectors, clusterer, "Bisecting k-means")
  }

  //---------------------------

  println(s"Experiment count: ${experiments.size}")

  val experimentsRun = experimentService.runExperiments(5)(experiments)
  val successfulExperimentsCount = Await.result(experimentsRun, Duration.Inf)

  println(s"Successful experiments: ${successfulExperimentsCount}/${experiments.size}")
}
