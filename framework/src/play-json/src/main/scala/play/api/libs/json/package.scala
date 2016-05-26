/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs

/**
 * Json API
 * For example:
 * {{{
 * import play.api.libs.json._
 * import play.api.libs.functional.syntax._
 *
 * case class User(id: Long, name: String, friends: Seq[User] = Seq.empty)
 * object User {
 *
 *   // In this format, an undefined friends property is mapped to an empty list
 *   implicit val format: Format[User] = (
 *     (__ \ "id").format[Long] and
 *     (__ \ "name").format[String] and
 *     (__ \ "friends").lazyFormatNullable(implicitly[Format[Seq[User]]])
 *       .inmap[Seq[User]](_ getOrElse Seq.empty, Some(_))
 *   )(User.apply, unlift(User.unapply))
 * }
 *
 * //then in a controller:
 *
 * object MyController extends Controller {
 *    def displayUserAsJson(id: String) = Action {
 *       Ok(Json.toJson(User(id.toLong, "myName")))
 *    }
 *    def saveUser(jsonString: String)= Action {
 *      val user = Json.parse(jsonString).as[User]
 *      myDataStore.save(user)
 *      Ok
 *    }
 * }
 * }}}
 */
package object json {
  import scala.json.ast._

  /**
   * Alias for `JsPath` companion object
   */
  val __ = JsPath

  type JsValue = JValue
  type JsString = JString
  val JsString = JString
  val JsNull = JNull
  type JsNumber = JNumber
  val JsNumber = JNumber
  type JsArray = JArray
  val JsArray = JArray
  type JsObject = JObject
  val JsObject = JObject
  type JsBoolean = JBoolean
  val JsBoolean = JBoolean

  implicit class jsObjectHelper(jsObject: JsObject) {
    lazy val fields: Seq[(String, JsValue)] = jsObject.value.toSeq

    /**
     * Return all fields as a set
     */
    def fieldSet: Set[(String, JsValue)] = fields.toSet

    /**
     * Return all keys
     */
    def keys: Set[String] = jsObject.value.keySet

    /**
     * Return all values
     */
    def values: Iterable[JsValue] = jsObject.value.values

    /**
     * Merge this object with another one. Values from other override value of the current object.
     */
    def ++(other: JsObject): JsObject = JsObject(jsObject.value ++ other.value)

    /**
     * Removes one field from the JsObject
     */
    def -(otherField: String): JsObject = JsObject(jsObject.value - otherField)

    /**
     * Adds one field to the JsObject
     */
    def +(otherField: (String, JsValue)): JsObject = JsObject(jsObject.value + otherField)

    /**
     * merges everything in depth and doesn't stop at first level, as ++ does
     */
    def deepMerge(other: JsObject): JsObject = {
      def merge(existingObject: JsObject, otherObject: JsObject): JsObject = {
        val result = existingObject.value ++ otherObject.value.map {
          case (otherKey, otherValue) =>
            val maybeExistingValue = existingObject.value.get(otherKey)

            val newValue = (maybeExistingValue, otherValue) match {
              case (Some(e: JsObject), o: JsObject) => merge(e, o)
              case _ => otherValue
            }
            otherKey -> newValue
        }
        JsObject(result)
      }
      merge(jsObject, other)
    }
  }

  implicit class jsArrayHelper(jsArray: JsArray) {
    def ++(other: JsArray): JsArray =
      JsArray(jsArray.value ++ other.value)

    /**
     * Append an element to this array.
     */
    def :+(el: JsValue): JsArray = JsArray(jsArray.value :+ el)
    def append(el: JsValue): JsArray = this.:+(el)

    /**
     * Prepend an element to this array.
     */
    def +:(el: JsValue): JsArray = JsArray(el +: jsArray.value)
    def prepend(el: JsValue): JsArray = this.+:(el)
  }

  implicit class jsValueHelper(jsValue: JsValue) extends JsReadable {
    override def toString = Json.stringify(jsValue)

    def validate[A](implicit rds: Reads[A]): JsResult[A] = rds.reads(jsValue)

    def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]] = JsDefined(jsValue).validateOpt[A]
  }

  implicit def jsValueToJsLookup(value: JsValue): JsLookup = JsLookup(JsDefined(value))
}
