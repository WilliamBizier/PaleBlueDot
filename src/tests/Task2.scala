package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1:Countries with normal caps") {

    val testCases: Map[String, Double] = Map(
        "Andorra" -> 8409.5,
        "United Arab Emirates" -> 761668.3333333334,
        "Afghanistan" -> 99154.21052631579

      )

      for ((input, expectedOutput) <- testCases) {
        val computedOutput: Double = PaleBlueDot.averagePopulation(countriesFile, citiesFilename,input)
        assert(Math.abs(computedOutput-expectedOutput)<0.001, input+"->"+computedOutput)
      }



    }
  test("test 2:Countries with random caps") {

    val testCases: Map[String, Double] = Map(
      "ANDorra" -> 8409.5,
      "United aRAB Emirates" -> 761668.3333333334,
      "AfghanIStan" -> 99154.21052631579

    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Double = PaleBlueDot.averagePopulation(countriesFile, citiesFilename, input)
      assert(Math.abs(computedOutput - expectedOutput) < 0.001, input + "->" + computedOutput)
    }



  }


}