package org.thp.elastictool

import play.api.libs.json.JsObject

import scala.util.matching.Regex

class ExtendedIndex(
    val index: Index,
    val mapping: JsObject,
    val settings: JsObject,
    val versionCreated: String,
    val versionUpgraded: Option[String]
) {
  override def toString: String =
    s"$index version=${ExtendedIndex.convertVersion(versionCreated)}${versionUpgraded.fold("")(i ⇒ s"->${ExtendedIndex.convertVersion(i)}")}"
}

object ExtendedIndex {
  val versionPattern: Regex = """(\d)(\d{2})(\d{2})(\d{2})""".r

  def convertVersion(v: String): String = v match {
    case versionPattern(major, minor, revision, "99") ⇒ s"${major.toInt}.${minor.toInt}.${revision.toInt}"
    case versionPattern(major, minor, revision, revType) if revType.toInt < 50 ⇒
      s"${major.toInt}.${minor.toInt}.${revision.toInt}-beta${revType.toInt}"
    case versionPattern(major, minor, revision, revType) ⇒ s"${major.toInt}.${minor.toInt}.${revision.toInt}-RC${revType.toInt - 49}"
    case other                                           ⇒ other
  }

}
