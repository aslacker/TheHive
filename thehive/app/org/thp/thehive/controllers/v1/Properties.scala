package org.thp.thehive.controllers.v1

import javax.inject.{Inject, Singleton}
import org.thp.scalligraph.BadRequestError
import org.thp.scalligraph.controllers.FPathElem
import org.thp.scalligraph.models.{IdMapping, UMapping}
import org.thp.scalligraph.query.{PublicProperty, PublicPropertyListBuilder}
import org.thp.scalligraph.traversal.TraversalOps._
import org.thp.thehive.models._
import org.thp.thehive.services.AlertOps._
import org.thp.thehive.services.AuditOps._
import org.thp.thehive.services.CaseOps._
import org.thp.thehive.services.CaseTemplateOps._
import org.thp.thehive.services.LogOps._
import org.thp.thehive.services.ObservableOps._
import org.thp.thehive.services.TagOps._
import org.thp.thehive.services.TaskOps._
import org.thp.thehive.services.UserOps._
import org.thp.thehive.services._
import play.api.libs.json.{JsObject, Json}

import scala.util.Failure

@Singleton
class Properties @Inject() (
    alertSrv: AlertSrv,
    caseSrv: CaseSrv,
    taskSrv: TaskSrv,
    userSrv: UserSrv,
    caseTemplateSrv: CaseTemplateSrv,
    observableSrv: ObservableSrv
) {

  lazy val alert: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Alert]
      .property("type", UMapping.string)(_.field.updatable)
      .property("source", UMapping.string)(_.field.updatable)
      .property("sourceRef", UMapping.string)(_.field.updatable)
      .property("title", UMapping.string)(_.field.updatable)
      .property("description", UMapping.string)(_.field.updatable)
      .property("severity", UMapping.int)(_.field.updatable)
      .property("date", UMapping.date)(_.field.updatable)
      .property("lastSyncDate", UMapping.date.optional)(_.field.updatable)
      .property("tags", UMapping.string.set)(
        _.select(_.tags.displayName)
          .custom { (_, value, vertex, _, graph, authContext) =>
            alertSrv
              .get(vertex)(graph)
              .getOrFail("Alert")
              .flatMap(alert => alertSrv.updateTagNames(alert, value)(graph, authContext))
              .map(_ => Json.obj("tags" -> value))
          }
      )
      .property("flag", UMapping.boolean)(_.field.updatable)
      .property("tlp", UMapping.int)(_.field.updatable)
      .property("pap", UMapping.int)(_.field.updatable)
      .property("read", UMapping.boolean)(_.field.updatable)
      .property("follow", UMapping.boolean)(_.field.updatable)
      .property("read", UMapping.boolean)(_.field.updatable)
      .property("imported", UMapping.boolean)(_.select(_.imported).readonly)
      .property("summary", UMapping.string.optional)(_.field.updatable)
      .property("user", UMapping.string)(_.field.updatable)
      .property("customFields", UMapping.jsonNative)(_.subSelect {
        case (FPathElem(_, FPathElem(name, _)), alertSteps) => alertSteps.customFields(name).jsonValue
        case (_, alertSteps)                                => alertSteps.customFields.nameJsonValue.fold.domainMap(JsObject(_))
      }.custom {
        case (FPathElem(_, FPathElem(name, _)), value, vertex, _, graph, authContext) =>
          for {
            c <- alertSrv.getOrFail(vertex)(graph)
            _ <- alertSrv.setOrCreateCustomField(c, name, Some(value))(graph, authContext)
          } yield Json.obj(s"customField.$name" -> value)
        case _ => Failure(BadRequestError("Invalid custom fields format"))
      })
      .build

  lazy val audit: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Audit]
      .property("operation", UMapping.string)(_.rename("action").readonly)
      .property("details", UMapping.string)(_.field.readonly)
      .property("objectType", UMapping.string.optional)(_.field.readonly)
      .property("objectId", UMapping.string.optional)(_.field.readonly)
      .property("base", UMapping.boolean)(_.rename("mainAction").readonly)
      .property("startDate", UMapping.date)(_.rename("_createdAt").readonly)
      .property("requestId", UMapping.string)(_.field.readonly)
      .property("rootId", IdMapping)(_.select(_.context._id).readonly)
      .build

  lazy val `case`: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Case]
      .property("title", UMapping.string)(_.field.updatable)
      .property("description", UMapping.string)(_.field.updatable)
      .property("severity", UMapping.int)(_.field.updatable)
      .property("startDate", UMapping.date)(_.field.updatable)
      .property("endDate", UMapping.date.optional)(_.field.updatable)
      .property("tags", UMapping.string.set)(
        _.select(_.tags.displayName)
          .custom { (_, value, vertex, _, graph, authContext) =>
            caseSrv
              .get(vertex)(graph)
              .getOrFail("Case")
              .flatMap(`case` => caseSrv.updateTagNames(`case`, value)(graph, authContext))
              .map(_ => Json.obj("tags" -> value))
          }
      )
      .property("flag", UMapping.boolean)(_.field.updatable)
      .property("tlp", UMapping.int)(_.field.updatable)
      .property("pap", UMapping.int)(_.field.updatable)
      .property("status", UMapping.enum[CaseStatus.type])(_.field.updatable)
      .property("summary", UMapping.string.optional)(_.field.updatable)
      .property("assignee", UMapping.string.optional)(_.select(_.user.value(_.login)).custom { (_, login, vertex, _, graph, authContext) =>
        for {
          c    <- caseSrv.get(vertex)(graph).getOrFail("Case")
          user <- login.map(userSrv.get(_)(graph).getOrFail("User")).flip
          _ <- user match {
            case Some(u) => caseSrv.assign(c, u)(graph, authContext)
            case None    => caseSrv.unassign(c)(graph, authContext)
          }
        } yield Json.obj("owner" -> user.map(_.login))
      })
      .property("impactStatus", UMapping.string.optional)(_.select(_.impactStatus.value(_.value)).custom {
        (_, value, vertex, _, graph, authContext) =>
          caseSrv
            .get(vertex)(graph)
            .getOrFail("Case")
            .flatMap { c =>
              value.fold(caseSrv.unsetImpactStatus(c)(graph, authContext))(caseSrv.setImpactStatus(c, _)(graph, authContext))
            }
            .map(_ => Json.obj("impactStatus" -> value))
      })
      .property("resolutionStatus", UMapping.string.optional)(_.select(_.resolutionStatus.value(_.value)).custom {
        (_, value, vertex, _, graph, authContext) =>
          caseSrv
            .get(vertex)(graph)
            .getOrFail("Case")
            .flatMap { c =>
              value.fold(caseSrv.unsetResolutionStatus(c)(graph, authContext))(caseSrv.setResolutionStatus(c, _)(graph, authContext))
            }
            .map(_ => Json.obj("resolutionStatus" -> value))
      })
      .build

  lazy val caseTemplate: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[CaseTemplate]
      .property("name", UMapping.string)(_.field.updatable)
      .property("displayName", UMapping.string)(_.field.updatable)
      .property("titlePrefix", UMapping.string.optional)(_.field.updatable)
      .property("description", UMapping.string.optional)(_.field.updatable)
      .property("severity", UMapping.int.optional)(_.field.updatable)
      .property("tags", UMapping.string.set)(
        _.select(_.tags.displayName)
          .custom { (_, value, vertex, _, graph, authContext) =>
            caseTemplateSrv
              .get(vertex)(graph)
              .getOrFail("CaseTemplate")
              .flatMap(caseTemplate => caseTemplateSrv.updateTagNames(caseTemplate, value)(graph, authContext))
              .map(_ => Json.obj("tags" -> value))
          }
      )
      .property("flag", UMapping.boolean)(_.field.updatable)
      .property("tlp", UMapping.int.optional)(_.field.updatable)
      .property("pap", UMapping.int.optional)(_.field.updatable)
      .property("summary", UMapping.string.optional)(_.field.updatable)
      .property("user", UMapping.string)(_.field.updatable)
      .property("customFields", UMapping.jsonNative)(_.subSelect {
        case (FPathElem(_, FPathElem(name, _)), alertSteps) => alertSteps.customFields(name).jsonValue
        case (_, alertSteps)                                => alertSteps.customFields.nameJsonValue.fold.domainMap(JsObject(_))
      }.custom {
        case (FPathElem(_, FPathElem(name, _)), value, vertex, _, graph, authContext) =>
          for {
            c <- caseTemplateSrv.getOrFail(vertex)(graph)
            _ <- caseTemplateSrv.setOrCreateCustomField(c, name, Some(value), None)(graph, authContext)
          } yield Json.obj(s"customField.$name" -> value)
        case _ => Failure(BadRequestError("Invalid custom fields format"))
      })
      .build

  lazy val organisation: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Organisation]
      .property("name", UMapping.string)(_.field.updatable)
      .property("description", UMapping.string)(_.field.updatable)
      .build

  lazy val profile: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Profile]
      .property("name", UMapping.string)(_.field.updatable)
      .property("permissions", UMapping.string.set)(_.field.updatable)
      .build

  lazy val task: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Task]
      .property("title", UMapping.string)(_.field.updatable)
      .property("description", UMapping.string.optional)(_.field.updatable)
      .property("status", UMapping.string)(_.field.updatable)
      .property("flag", UMapping.boolean)(_.field.updatable)
      .property("startDate", UMapping.date.optional)(_.field.updatable)
      .property("endDate", UMapping.date.optional)(_.field.updatable)
      .property("order", UMapping.int)(_.field.updatable)
      .property("dueDate", UMapping.date.optional)(_.field.updatable)
      .property("assignee", UMapping.string.optional)(_.select(_.assignee.value(_.login)).custom {
        case (_, value, vertex, _, graph, authContext) =>
          taskSrv
            .get(vertex)(graph)
            .getOrFail("Task")
            .flatMap { task =>
              value.fold(taskSrv.unassign(task)(graph, authContext)) { user =>
                userSrv
                  .get(user)(graph)
                  .getOrFail("User")
                  .flatMap(taskSrv.assign(task, _)(graph, authContext))
              }
            }
            .map(_ => Json.obj("assignee" -> value))
      })
      .build

  lazy val log: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Log]
      .property("message", UMapping.string)(_.field.updatable)
      .property("deleted", UMapping.boolean)(_.field.updatable)
      .property("date", UMapping.date)(_.field.readonly)
      .property("attachment", IdMapping)(_.select(_.attachments._id).readonly)
      .build

  lazy val user: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[User]
      .property("login", UMapping.string)(_.field.readonly)
      .property("name", UMapping.string)(_.field.readonly)
      .property("locked", UMapping.boolean)(_.field.readonly)
      .property("avatar", UMapping.string.optional)(_.select(_.avatar.value(_.attachmentId).domainMap(id => s"/api/datastore/$id")).readonly)
      .build

  lazy val observable: List[PublicProperty[_, _]] =
    PublicPropertyListBuilder[Observable]
      .property("status", UMapping.string)(_.select(_.constant("Ok")).readonly)
      .property("startDate", UMapping.date)(_.select(_._createdAt).readonly)
      .property("ioc", UMapping.boolean)(_.field.updatable)
      .property("sighted", UMapping.boolean)(_.field.updatable)
      .property("tags", UMapping.string.set)(
        _.select(_.tags.displayName)
          .custom { (_, value, vertex, _, graph, authContext) =>
            observableSrv
              .getOrFail(vertex)(graph)
              .flatMap(observable => observableSrv.updateTagNames(observable, value)(graph, authContext))
              .map(_ => Json.obj("tags" -> value))
          }
      )
      .property("message", UMapping.string)(_.field.updatable)
      .property("tlp", UMapping.int)(_.field.updatable)
      .property("dataType", UMapping.string)(_.select(_.observableType.value(_.name)).readonly)
      .property("data", UMapping.string.optional)(_.select(_.data.value(_.data)).readonly)
      // TODO add attachment ?
      .build
}
