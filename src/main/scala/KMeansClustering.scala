/**
  * Created by yoonjeonghun on 2017. 7. 4..
  */

import Math._

class KMeansClustering {

  // cluster 를 구한다.
  def getClusters(inputData: Array[Double], centroids: Array[Double]): Array[Int] = {
    val dataLength = inputData.length
    val k: Int = centroids.length
    // Cluster 의 index 정보를 가진 배열을 생성한다. 초기값은 0이지만, 새로운 centroid 값으로 계속 update 될 것이다.
    var assignments: Array[Int] = Array.fill(dataLength)(-1)
    var newAssignments: Array[Int] = Array.fill(dataLength)(-1)

    // input data 에 최소거리의 centroid 를 assign 한다.
    for ((data, i) <- inputData.zipWithIndex) {
      newAssignments(i) = findMinCentroid(data, centroids)
    }

    // assigned cluster 가 변화가 있는지 체크
    while (!isEqual(assignments, newAssignments)) {
      // assignments 를 복사한 후, newAssignments 는 초기화
      assignments = newAssignments
      newAssignments = Array.fill(dataLength)(-1)

      // centroids 는 update
      val newCentroids = getNewCentroids(inputData, assignments, k)

      for ((data, i) <- inputData.zipWithIndex) {
        newAssignments(i) = findMinCentroid(data, newCentroids)
      }
    }
    newAssignments
  }


  // Calculating new centroids
  def getNewCentroids(inputData: Array[Double], assignments: Array[Int], k: Int): Array[Double] = {
    val centroids: Array[Double] = Array.fill(k)(0)
    // centroid 에 assign 된 data 의 수를 알아야 한다.
    val numOfAssigned: Array[Int] = Array.fill(k)(0)

    (inputData zip assignments).foreach(
      elem => {
        centroids(elem._2) += elem._1
        numOfAssigned(elem._2) += 1
      }
    )

    // 평균을 구한다.
    (centroids zip numOfAssigned).map(
      elem => elem._1 / elem._2.toDouble
    )
  }


  // Checking the equality of two arrays
  def isEqual(arr1: Array[Int], arr2: Array[Int]): Boolean = {
    (arr1 zip arr2).foreach(
      elem =>
        if (elem._1 != elem._2) {
          return false
        }
    )
    true
  }


  // data 와 centroid 의 거리를 계산한 후, 최소거리의 centroid 를 구한다.
  def findMinCentroid(data: Double, centroids: Array[Double]): Int = {
    var distance: Double = 0
    var minDistance: Double = Double.MaxValue
    var index: Int = -1

    for (i <- centroids.indices) {
      distance = getDistance(data, centroids(i))
      if (distance < minDistance) {
        minDistance = distance
        index = i
      }
    }
    index
  }


  // 유클리디안 거리를 구한다.
  def getDistance(p1: Double, p2: Double): Double = {
    sqrt(pow(p1 - p2, 2))
  }
}