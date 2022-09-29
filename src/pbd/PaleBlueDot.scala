package pbd

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}

object PaleBlueDot {


  /**
   * Task 1
   *
   * Given a country name using a mix of case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename and the countryName input
   * of your method is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName in lowercase letters
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    val countriesFile: BufferedSource = Source.fromFile(countriesFilename)
    var alltolower: String = countryName.toLowerCase()
    for (name <- countriesFile.getLines()) {
      val getcode: Array[String] = name.split("#")
      var bigname: String = getcode(0).toLowerCase()
      var code: String = getcode(1)
      if (alltolower == bigname) {
        return code.toLowerCase()
      }
    }
    ""
  }


  /**
   * Task 2
   *
   * Find the average population of cities in a country
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The average population of cities in the given country
   */
  def averagePopulation(countriesFilename: String, citiesFilename: String, countryName: String): Double = {
    var task1: String = getCountryCode(countriesFilename, countryName)
    val cities: BufferedSource = Source.fromFile(citiesFilename)
    var popgrosstotal: Double = 0.0
    var totalcitylitty: Double = 0.0
    for (lines <- cities.getLines()) {
      var splitty: Array[String] = lines.split(",")
      if (task1 == splitty(0)) {
        //anything in csv file is a string must convert to desired type
        popgrosstotal += splitty(3).toDouble
        totalcitylitty += 1
      }
    }
    if (totalcitylitty == 0) {
      totalcitylitty = 1
    }
    return popgrosstotal / totalcitylitty
  }


  /**
   * Task 3
   */

  /**
   * Returns a Map[cityName -> population] for all cities in the given county. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int. The country name may contain any mix of upper/lower-case letters.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return A Map containing the name and population of every city in the given country
   */
  def cityPopulations(countriesFilename: String, citiesFilename: String, countryName: String): Map[String, Int] = {
    var task1: String = getCountryCode(countriesFilename, countryName)
    val cities: BufferedSource = Source.fromFile(citiesFilename)
    var themap: Map[String, Int] = Map()
    println("code:" + task1)
    for (lines <- cities.getLines()) {
      var splitty: Array[String] = lines.split(",")
      if (task1 == splitty(0)) {
        themap += (splitty(1) -> splitty(3).toInt)

      }
    }
    themap
  }


  /**
   * Returns a List of city names in the given county and with above average population for that country
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return All city names in given country with a population > the average populations of cities in that country
   */
  def aboveAverageCities(countriesFilename: String, citiesFilename: String, countryName: String): List[String] = {
    var mapofcities: Map[String, Int] = cityPopulations(countriesFilename, citiesFilename, countryName)
    var averagepop: Double = averagePopulation(countriesFilename, citiesFilename, countryName)
    var megachad: List[String] = List()
    for ((key, value) <- mapofcities) {
      if (value.toDouble > averagepop) {
        megachad = megachad :+ key
      }
    }
    megachad
  }


  /**
   * Application Objective
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * Return the closest city to the given location in terms of greater circle distance which is the shortest distance
   * needed to walk along the surface of the Earth to reach a city.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file (ie. the List should have exactly 3 values to return
   *         a single city
   */

  import scala.collection.immutable.ListMap
  //this allows for the sorting of a list

  def closestCity(citiesFilename: String, location: List[Double]): List[String] = {
    List("Country Code", "City Name", "Region")
    val cities: BufferedSource = Source.fromFile(citiesFilename)
    var radiusvalues: Map[Double, List[String]] = Map()
    var mapkey: Double = 1000000000
    for (lines <- cities.getLines().drop(1)) {
      var splitty: Array[String] = lines.split(",").map(_.trim)
      val R: Double  = 6371.0 // metres
      var step1:Double = location.head * Math.PI/180 // φ, λ in radians
      var step2:Double = splitty(4).toDouble * Math.PI/180
      var step3:Double = (location.head-splitty(4).toDouble) * Math.PI/180
      var step4:Double = (location(1)-splitty(5).toDouble) * Math.PI/180
      var a:Double = Math.sin(step3/2) * Math.sin(step3/2) + Math.cos(step1) * Math.cos(step2) * Math.sin(step4/2) * Math.sin(step4/2)
      var c:Double = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
      var d:Double = R * c // in metres
      if (d < mapkey){
        mapkey = d
      }
      radiusvalues = radiusvalues + (d -> List(splitty(0), splitty(1), splitty(2)))
    }
    radiusvalues = ListMap(radiusvalues.toSeq.sortWith(_._1 < _._1): _*)
    return radiusvalues(mapkey)
  }



  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }

//scala only runs whatcha got in main




  import scala.collection.immutable.ListMap


  def main(args: Array[String]): Unit = {
    println(getCountryCode("data/countries.txt","ARubA"))
    println(getCountryCode("data/countries.txt","ChiNA"))
    println(getCountryCode("data/countries.txt","FrAnCE"))
    println(getCountryCode("data/countries.txt","UrMoM"))
    //openMap(List(43.002743, -78.7874136))
    //Gibraltar
    println(averagePopulation("data/countries.txt","data/cities.csv","Andorra"))
    println(aboveAverageCities(countriesFilename="data/countries.txt",citiesFilename = "data/cities.csv" ,countryName = "Gibraltar"))
    println(aboveAverageCities(countriesFilename="data/countries.txt",citiesFilename = "data/cities.csv" ,countryName = "Zambia"))
    println(cityPopulations(countriesFilename="data/countries.txt",citiesFilename = "data/cities.csv" ,countryName = "Monaco"))
    println(scala.math.pow((64),0.5))
    var cock:Map[Double,String] = Map(2.1 -> "234",100.0 -> "234",50.0 -> "234",75.0 -> "234",99.0 -> "234")
    cock = ListMap(cock.toSeq.sortWith(_._1 < _._1):_*)
    println(cock.toString())
    println(closestCity(citiesFilename = "data/cities.csv",location = List(90.0,45.0)))


  }

}
