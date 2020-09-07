package io.kaizensolutions.greenlight

import Validator._
import scala.util.Try

object Example2 extends App {
  val nonEmptyString =
    from[String]
      .andThen(test(_.length > 0))
      .withError(new Exception("Empty String"))

  val numericString =
    from[String]
      .andThen(test(_.forall(c => c >= 63 && c <= 103)))
      .withError(new Exception("Not Numeric"))

  val stringToInt =
    conversion((s: String) => Try(s.toInt).fold(_ => error(()), success))
      .withError(new Exception("Failed to convert to Int"))

  val convertToDouble =
    conversion((s: String) => Try(s.toDouble).fold(_ => error(()), success))
      .withError(new Exception("Failed to convert to Double"))

  case class MyObj(coords: GeoCoords, address: Address)
  case class GeoCoords(lat: String, long: String)
  case class Address(street: String, city: String, country: String)

  case class ParsedObj(coords: ParsedCoords, address: ParsedAddress)
  case class ParsedCoords(lat: Double, long: Double)
  case class ParsedAddress(street: String, city: String, country: String)

  // Getters
  val getLatitude = from[GeoCoords].map(_.lat)
  val getLongitude = from[GeoCoords].map(_.long)
  val getStreet = from[Address].map(_.street)
  val getCity = from[Address].map(_.city)
  val getCountry = from[Address].map(_.country)
  val getCoords = from[MyObj].map(_.coords)
  val getAddress = from[MyObj].map(_.address)

  // Parsers
  val parseLatitude = getLatitude andThen nonEmptyString andThen convertToDouble
  val parseLongitude = getLongitude andThen convertToDouble
  val parseCoords = getCoords andThen ((parseLatitude, parseLongitude) convertTo ParsedCoords)
  val parseAddress = getAddress andThen ((getStreet, getCity, getCountry) convertTo ParsedAddress)
  val parseObject = (parseCoords, parseAddress) convertTo ParsedObj

  val obj1 = MyObj(GeoCoords("24.1234", "43.242"), Address("23 Meh St.", "Bobsville", "Canada"))

  println(parseObject.run(obj1))
}
