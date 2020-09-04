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
    conversion((s: String) => Try(s.toInt).fold(_ => failure(()), success))
      .withError(new Exception("Failed to convert to Int"))

  val convertToDouble =
    conversion((s: String) => Try(s.toDouble).fold(_ => failure(()), success))
      .withError(new Exception("Failed to convert to Double"))

  case class MyObj(coords: GeoCoords, address: Address)
  case class GeoCoords(lat: String, long: String)
  case class Address(street: String, city: String, country: String)

  case class ParsedObj(coords: ParsedCoords, address: ParsedAddress)
  case class ParsedCoords(lat: Double, long: Double)
  case class ParsedAddress(street: String, city: String, country: String)

  // Getters
  val getLatitude: Validator[GeoCoords, Nothing, Nothing, String] = from[GeoCoords].map(_.lat)
  val getLongitude: Validator[GeoCoords, Nothing, Nothing, String] = from[GeoCoords].map(_.long)
  val getStreet: Validator[Address, Nothing, Nothing, String] = from[Address].map(_.street)
  val getCity: Validator[Address, Nothing, Nothing, String] = from[Address].map(_.city)
  val getCountry: Validator[Address, Nothing, Nothing, String] = from[Address].map(_.country)
  val getCoords: Validator[MyObj, Nothing, Nothing, GeoCoords] = from[MyObj].map(_.coords)
  val getAddress: Validator[MyObj, Nothing, Nothing, Address] = from[MyObj].map(_.address)

  // Parsers
  val parseLatitude: Validator[GeoCoords, Nothing, Exception, Double] = (getLatitude andThen nonEmptyString andThen convertToDouble) or success(0d)
  val parseLongitude = getLongitude andThen convertToDouble
  val parseCoords = getCoords andThen ((parseLatitude, parseLongitude) convertTo ParsedCoords)
  val parseAddress = getAddress andThen ((getStreet, getCity, getCountry) convertTo ParsedAddress)
  val parseObject = (parseCoords, parseAddress) convertTo ParsedObj

  val obj1 = MyObj(GeoCoords("24.1234", "43.242"), Address("23 Meh St.", "Bobsville", "Canada"))

  println(parseObject.run(obj1))
}
