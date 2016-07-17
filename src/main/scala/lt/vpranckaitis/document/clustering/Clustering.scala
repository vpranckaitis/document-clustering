package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.clusterers.kmeans.{ClassicKMeans, DistanceFunction, InitialMeans}
import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Clustering extends App {

  val experimentService = new ExperimentService(new Storage)

  val future = experimentService.runExperiment(2) { articles =>

    val featureVectors =
      FeatureSelection(articles).
        takeText().
        split().
        toLowercase().
        lengthAtLeast(3).
        stem().
        termFrequencyInverseDocumentFrequency().
        //leaveTermsWithHighestValues(0.25).
        leaveNHighestTerms(100).
        normalize().
        toFeatureVectors()

    val clusterer = new ClassicKMeans(10, DistanceFunction.Cosine, InitialMeans.Random(999999))

    (featureVectors, clusterer)
  }

  Await.ready(future, Duration.Inf)
}
