package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.ClusteringEvaluation.PositivesAndNegatives
import org.scalatest.{FlatSpec, Matchers}

class ClusteringEvaluationSpec extends FlatSpec with Matchers {
  ".positivesAndNegatives" should "produce correct values" in {
    // http://nlp.stanford.edu/IR-book/html/htmledition/evaluation-of-clustering-1.html

    val clusters = Seq(
      Seq("x" -> 5, "o" -> 1),
      Seq("x" -> 1, "o" -> 4, "z" -> 1),
      Seq("x" -> 2, "z" -> 3)
    )

    val expected = PositivesAndNegatives(tp = 20, tn = 72, fp = 20, fn = 24)

    ClusteringEvaluation.positivesAndNegatives(clusters) should equal (expected)
  }


}
