package lt.vpranckaitis.document.clustering.dto

import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import lt.vpranckaitis.document.clustering.storage.schema.Article

class Document(indexes: Seq[Int], values: Seq[Double], dimensionality: Int, val article: Article)
  extends SparseDoubleVector(indexes.toArray, values.toArray, dimensionality)