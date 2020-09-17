package io.kaizensolutions.nigelv1

import io.kaizensolutions.nigelv1.Result.{Error, Success}
import io.kaizensolutions.nigelv1.Validator.{from, success, test}

import scala.util.Try

object Example extends App {
  val nonEmptyString =
    from[String]
      .andThen(test(_.length > 0))
      .asError(new Exception("Empty String"))

  val numericString =
    test[String](_.forall(c => c >= 63 && c <= 103))
      .withError((i: String) => new Exception(s"Value $i is not numeric"))

  val stringToInt =
    Validator
      .fromFallible((s: String) => Try(s.toInt))
      .withError((i: String) => new Exception(s"Failed to convert $i to Int"))

  val convertToDouble =
    Validator
      .fromFallible((s: String) => Try(s.toDouble))
      .withError((i: String) => new Exception(s"Failed to convert $i to Double"))

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


  val geo2parsedCoords =
    ((convertToDouble or success(0d)) and (convertToDouble or success(0d)))
      .contramap((coords: GeoCoords) => (coords.lat, coords.long))
      .map(ParsedCoords.tupled)

  val addr2parsedAddr =
    (from[String], from[String], from[String])
      .all
      .dimap(
        (addr: Address) => (addr.street, addr.city, addr.country),
        ParsedAddress.tupled
      )

  val parsedObj =
    (geo2parsedCoords and addr2parsedAddr)
      .contramap((o: MyObj) => (o.coords, o.address))
      .map(ParsedObj.tupled)


  val obj1 = MyObj(GeoCoords("23.123", "43.242"), Address("23 Meh St.", "Bobsville", "Canada"))

  parsedObj.run(obj1) match {
    case Success(warnings, result) =>
      println("Success!!!")
      println(s"Result: $result")
      println()
      println("Warnings:")
      warnings.foreach(println)

    case Error(warnings, errors) =>
      println("Failed :(")
      println("Errors:")
      errors.foreach(println)
      println()
      println("Warnings:")
      warnings.foreach(println)
  }
}
