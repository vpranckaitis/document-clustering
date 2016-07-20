package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.clusterers.em.EM
import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Clustering extends App {

  val experimentService = new ExperimentService(new Storage)

  val future = experimentService.consoleOutput.runExperiment(1) { articles =>

    val featureVectors =
      FeatureSelection(articles take 200).
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

    //val clusterer = new ClassicKMeans(10, DistanceFunction.Cosine, InitialMeans.Random(999999))
    val clusterer = new EM(10)

    (featureVectors, clusterer)
  }

  Await.result(future, Duration.Inf)
}
