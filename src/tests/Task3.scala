package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("1 - Country Names with Proper Capitilization") {

    val testCases: Map[String, Map[String, Int]] = Map(
      "Andorra" -> Map("la massana" -> 7211, "les escaldes" -> 15854, "ordino" -> 2553, "sant julia de loria" -> 8020))

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Map[String,Int] = PaleBlueDot.cityPopulations(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput)
    }

  }

//kulob, konibodom, uroteppa, tursunzoda, isfara, dushanbe, khujand, panjakent, kofarnihon
  test("2 - Country Names with one city") {

    val testCases: Map[String, List[String]] = Map(
      "Gibraltar" -> List())

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted)
    }
  }


    test("3 - hopefully correct and stuff") {

      val testCases: Map[String, List[String]] = Map(
        "Andorra" -> List("les escaldes"))

      for ((input, expectedOutput) <- testCases) {
        val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, input)
        assert(computedOutput.sorted == expectedOutput.sorted)
      }
    }
  test("4 - hopefully correct and stuff") {

    val testCases: Map[String, List[String]] = Map(
      "Zambia" -> List("luanshya", "kabwe", "mufulira", "chipata", "livingstone", "chingola", "ndola", "kitwe", "kasama", "lusaka", "mazabuka", "kalulushi"))

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted)
    }
  }
}