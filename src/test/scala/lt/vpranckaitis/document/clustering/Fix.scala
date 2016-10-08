package lt.vpranckaitis.document.clustering

import java.util.concurrent.Executors

import lt.vpranckaitis.document.clustering.storage.Storage
import org.scalatest.FlatSpec
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class Fix extends FlatSpec with ScalaFutures {
  val storage = new Storage

    "" should "fix news category" in {
    def extract(url: String) = {
      val urlParse = """http://([^/]+)/([^/]+)/([^/]+)/([^/]+).*""".r
      url match {
        case urlParse(_, _, _, subcategory) => subcategory
      }
    }

    val f = storage.getExperimentsByCategory("www.15min.lt", "zmones", "naujiena", 2000) flatMap { as =>
      val r = as map { a => a.copy(subcategory = extract(a.url)) }
      r foreach println
      Future.traverse(r){ a =>
        storage.update(a)
      }
    }

    Await.result(f, Duration.Inf)
  }
}
