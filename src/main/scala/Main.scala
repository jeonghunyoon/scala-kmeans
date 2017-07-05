/**
  * Created by yoonjeonghun on 2017. 7. 5..
  */
object Main {

  def main(args: Array[String]): Unit = {
    val inputData: Array[Double] = Array(1, 1, 1, 5, 6, 6, 7)
    val centroids : Array[Double] = Array(1, 7)
    val kMeansClustering = new KMeansClustering
    val clusters = kMeansClustering.getClusters(inputData, centroids)
    clusters.foreach(
      elem =>
        println(elem)
    )
  }
}
