package lt.vpranckaitis.documentclustering

import de.lmu.ifi.dbs.elki.data.SparseDoubleVector

class Document(indexes: Seq[Int], values: Seq[Double], dimensionality: Int, val category: String)
  extends SparseDoubleVector(indexes.toArray, values.toArray, dimensionality)