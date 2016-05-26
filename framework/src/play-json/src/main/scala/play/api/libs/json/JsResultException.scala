package play.api.libs.json

import scala.collection._

import play.api.data.validation.ValidationError

case class JsResultException(errors: Seq[(JsPath, Seq[ValidationError])]) extends RuntimeException("JsResultException(errors:%s)".format(errors))
