package io.kaizensolutions.calv1

import io.kaizensolutions.calv1.Validator.{ fromPredicate, fromTry }

import scala.util.Try

object Example extends App {
  val nonEmptyString: Validator[String, Exception, Nothing, String] =
    validate[String]
      .andThen(fromPredicate(_.length > 0))
      .asError(new Exception("Empty String"))

  val numericString: Validator[String, Exception, Nothing, String] =
    fromPredicate[String](_.forall(c => c >= 63 && c <= 103))
      .asErrorWithInput((i: String) => new Exception(s"Value $i is not numeric"))

  val stringToInt: Validator[String, Exception, Nothing, Int] =
    fromTry((s: String) => Try(s.toInt))
      .asErrorWithInput((i: String) => new Exception(s"Failed to convert $i to Int"))

  val convertToDouble: Validator[String, Exception, Nothing, Double] =
    fromTry((s: String) => Try(s.toDouble))
      .asErrorWithInput((i: String) => new Exception(s"Failed to convert $i to Double"))

  case class MyObj(coords: GeoCoords, address: Address)
  case class GeoCoords(lat: String, long: String)
  case class Address(street: String, city: String, country: String)

  case class ParsedObj(coords: ParsedCoords, address: ParsedAddress)
  case class ParsedCoords(lat: Double, long: Double)
  case class ParsedAddress(street: String, city: String, country: String)

  // Getters
  val getLatitude  = validate[GeoCoords].map(_.lat)
  val getLongitude = validate[GeoCoords].map(_.long)
  val getStreet    = validate[Address].map(_.street)
  val getCity      = validate[Address].map(_.city)
  val getCountry   = validate[Address].map(_.country)
  val getCoords    = validate[MyObj].map(_.coords)
  val getAddress   = validate[MyObj].map(_.address)

  // Parsers
  val parseLatitude: Validator[GeoCoords, Exception, Nothing, Double] =
    getLatitude andThen nonEmptyString andThen convertToDouble

  val parseLongitude = getLongitude >>> convertToDouble

  val parseCoords: Validator[MyObj, Exception, Nothing, ParsedCoords] =
    getCoords >>> (parseLatitude + parseLongitude).map { case (lat, lng) => ParsedCoords(lat, lng) }

  val parseAddress: Validator[MyObj, Exception, Nothing, ParsedAddress] =
    getAddress andThen (getStreet + getCity + getCountry).map {
      case street * city * country => ParsedAddress(street, city, country)
    }
  val parseObject = (parseCoords + parseAddress).map { case c * a => ParsedObj(c, a) }
}
