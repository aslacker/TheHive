package org.thp.thehive.dto.v0

import play.api.libs.json.{Format, Json, Writes}

case class InputShare(caseId: String, organisationName: String, profile: String)

object InputShare {
  implicit val writes: Writes[InputShare] = Json.writes[InputShare]
}

case class OutputShare(caseId: String, organisationName: String, profile: String)

object OutputShare {
  implicit val format: Format[OutputShare] = Json.format[OutputShare]
}
