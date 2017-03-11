package lt.vpranckaitis.document.clustering.dto

import de.lmu.ifi.dbs.elki.data.SparseDoubleVector
import lt.vpranckaitis.document.clustering.storage.schema.Article

class Document(val indexes: Seq[Int], val values: Seq[Double], val dimensionality: Int, val article: Article)
  extends SparseDoubleVector(indexes.toArray, values.toArray, dimensionality)