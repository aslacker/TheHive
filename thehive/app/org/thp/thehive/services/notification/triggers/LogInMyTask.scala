package org.thp.thehive.services.notification.triggers

import javax.inject.{Inject, Singleton}
import org.apache.tinkerpop.gremlin.structure.Graph
import org.thp.scalligraph.models.Entity
import org.thp.scalligraph.traversal.TraversalOps._
import org.thp.thehive.models.{Audit, Organisation, User}
import org.thp.thehive.services.LogOps._
import org.thp.thehive.services.LogSrv
import org.thp.thehive.services.TaskOps._
import play.api.Configuration

import scala.util.{Success, Try}

@Singleton
class LogInMyTaskProvider @Inject() (logSrv: LogSrv) extends TriggerProvider {
  override val name: String                               = "LogInMyTask"
  override def apply(config: Configuration): Try[Trigger] = Success(new LogInMyTask(logSrv))
}

class LogInMyTask(logSrv: LogSrv) extends Trigger {
  override val name: String = "LogInMyTask"

  override def preFilter(audit: Audit with Entity, context: Option[Entity], organisation: Organisation with Entity): Boolean =
    audit.action == Audit.create && audit.objectType.contains("Log")

  override def filter(audit: Audit with Entity, context: Option[Entity], organisation: Organisation with Entity, user: Option[User with Entity])(
      implicit graph: Graph
  ): Boolean =
    user.fold(false) { u =>
      super.filter(audit, context, organisation, user) &&
      preFilter(audit, context, organisation) &&
      u.login != audit._createdBy &&
      audit.objectId.fold(false)(taskAssignee(_).fold(false)(_ == u.login))
    }

  def taskAssignee(logId: String)(implicit graph: Graph): Option[String] = logSrv.getByIds(logId).task.assignee.value(_.login).headOption
}
