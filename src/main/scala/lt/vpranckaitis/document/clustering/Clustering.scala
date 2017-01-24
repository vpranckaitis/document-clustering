package lt.vpranckaitis.document.clustering

import java.io.BufferedReader

import lt.vpranckaitis.document.clustering.ExperimentService.Experiment
import lt.vpranckaitis.document.clustering.clusterers.gdbscan.{CorePredicate, Gdbscan, NeighborPredicate}
import lt.vpranckaitis.document.clustering.clusterers.hierarchical.{ExtractionMethod, Hierarchical, LinkageMethod}
import lt.vpranckaitis.document.clustering.clusterers.{DistanceFunction, random}
import lt.vpranckaitis.document.clustering.clusterers.kmeans.{BisectingKMeans, ClassicKMeans, InitialMeans}
import lt.vpranckaitis.document.clustering.clusterers.optics.{DeLiClu, Optics, OpticsXi}
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
    //xi <- 0.001 to 1 by 0.05
    //k <- 9 to 13
    //initialMeansAlgorithm <- Seq(InitialMeans.Random/*, InitialMeans.FarthestPoints, InitialMeans.KMeansPlusPlus*/)
    //seed <- Seq(45, 72468, 92438, 242, 85436)
    //initialMeans = initialMeansAlgorithm(seed)
    minSize <- Seq(110, 140)
    linkage <- Seq(LinkageMethod.Complete, LinkageMethod.GroupAverage/*, LinkageMethod.Single*/)
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

    //val clusterer = new BisectingKMeans(k, new ClassicKMeans(2, DistanceFunction.Cosine, initialMeans))
    val clusterer = new Hierarchical(DistanceFunction.Cosine, linkage, ExtractionMethod.HDBSCAN(minSize))
    //val clusterer = new ClassicKMeans(k, DistanceFunction.Cosine, initialMeans)
    //val clusterer = new RandomClustering(k, seed)
    //val clusterer = new Gdbscan(NeighborPredicate.Epsilon(eps, DistanceFunction.Cosine), CorePredicate.MinPoints(5))
    //val clusterer = new OpticsXi(new Optics(DistanceFunction.Cosine, Int.MaxValue, 5), xi)

    val comment = linkage match {
      case LinkageMethod.Single => "Hierarchical Single HDBSCAN"
      case LinkageMethod.GroupAverage => "Hierarchical GroupAverage HDBSCAN"
      case LinkageMethod.Complete => "Hierarchical Complete HDBSCAN"
    }

    (featureVectors, clusterer, comment)
  }

  //---------------------------

  println(s"Experiment count: ${experiments.size}")

  val experimentsRun = experimentService.runExperiments(5)(experiments)
  val successfulExperimentsCount = Await.result(experimentsRun, Duration.Inf)

  println(s"Successful experiments: ${successfulExperimentsCount}/${experiments.size}")
}
