package org.thp.thehive.controllers.v0

import org.specs2.mock.Mockito
import org.thp.scalligraph.controllers.{Entrypoint, Field}
import org.thp.scalligraph.models.Database
import org.thp.scalligraph.query.{ParamQuery, PublicProperty, QueryExecutor}
import org.thp.thehive.services._
import play.api.libs.json.Json
import play.api.test.PlaySpecification

class QueryTest extends PlaySpecification with Mockito {

  val properties = new Properties(
    mock[CaseSrv],
    mock[UserSrv],
    mock[AlertSrv],
    mock[DashboardSrv],
    mock[ObservableSrv],
    mock[CaseTemplateSrv],
    mock[TaskSrv],
    mock[CustomFieldSrv],
    mock[Database]
  )

  val taskCtrl = new TaskCtrl(
    mock[Entrypoint],
    mock[Database],
    properties,
    mock[TaskSrv],
    mock[CaseSrv],
    mock[UserSrv],
    mock[OrganisationSrv],
    mock[ShareSrv]
  )

  val queryExecutor: QueryExecutor = new QueryExecutor {
    override val db: Database                                      = mock[Database]
    override val version: (Int, Int)                               = 0 -> 0
    override lazy val queries: Seq[ParamQuery[_]]                  = Seq(taskCtrl.initialQuery, taskCtrl.pageQuery, taskCtrl.outputQuery)
    override lazy val publicProperties: List[PublicProperty[_, _]] = taskCtrl.publicProperties
  }
  val queryCtrl: QueryCtrl = new QueryCtrlBuilder(mock[Entrypoint], mock[Database]).apply(taskCtrl, queryExecutor)

  "Controller" should {
    "parse stats query" in {
      val input = Json.parse("""
                               | {
                               |   "query": {
                               |     "_and": [{
                               |       "_in": {
                               |         "_field": "status",
                               |         "_values": ["Waiting", "InProgress"]
                               |       }
                               |     }, {
                               |       "owner": "admin@thehive.local"
                               |     }]
                               |   },
                               |   "stats": [{
                               |     "_agg": "field",
                               |     "_field": "status",
                               |     "_select": [{ "_agg": "count"}]
                               |   }, {
                               |     "_agg": "count"
                               |   }]
                               | }
        """.stripMargin)

      val queryOrError = queryCtrl.statsParser(Field(input))
      queryOrError.isGood must beTrue.updateMessage(s => s"$s\n$queryOrError")
      queryOrError.get must not be empty
    }
  }
}
