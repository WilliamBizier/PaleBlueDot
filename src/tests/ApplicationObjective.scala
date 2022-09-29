package tests

import org.scalatest._
import pbd.PaleBlueDot

class ApplicationObjective extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1:make sure the shit work") {

    val testCases: Map[List[Double], List[String]] = Map(
      List(42.55,1.5166667) -> List("ad","la massana","04")

    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.closestCity(citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted)


    }
  }
  test("test 1:North and South Pole") {

    val testCases: Map[List[Double], List[String]] = Map(
      List(90.0,135.0) -> List("sj", "ny-alesund", "00"),
      List(90.0,45.0) -> List("sj", "ny-alesund", "00")

    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.closestCity(citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted)


    }
  }
}