package org.thp.thehive.services
import java.util.{Date, UUID}

import scala.reflect.runtime.{universe ⇒ ru}

import gremlin.scala._
import javax.inject.{Inject, Provider, Singleton}
import org.thp.scalligraph.auth.AuthContext
import org.thp.scalligraph.controllers.UpdateOps
import org.thp.scalligraph.models.Model.Base
import org.thp.scalligraph.models._
import org.thp.scalligraph.query.PublicProperty
import org.thp.scalligraph.services.{EdgeSrv, ElementSrv}
import org.thp.scalligraph.{BadRequestError, FPath, ParentProvider}
import org.thp.thehive.models.{Audit, AuditableAction, Audited}

@Singleton
class AuditedDatabase @Inject()(originalDatabase: ParentProvider[Database], auditSrvProvider: Provider[AuditSrv]) extends Database {
  implicit lazy val db: Database = originalDatabase.get().get
  lazy val auditSrv: AuditSrv    = auditSrvProvider.get()

  override val idMapping: SingleMapping[UUID, String]            = db.idMapping
  override val createdAtMapping: SingleMapping[Date, Date]       = db.createdAtMapping
  override val createdByMapping: SingleMapping[String, String]   = db.createdByMapping
  override val updatedAtMapping: OptionMapping[Date, Date]       = db.updatedAtMapping
  override val updatedByMapping: OptionMapping[String, String]   = db.updatedByMapping
  override val binaryMapping: SingleMapping[Array[Byte], String] = db.binaryMapping

  lazy val edgeSrv: EdgeSrv[Audited, Audit, Product] = new EdgeSrv[Audited, Audit, Product]
  def create(audit: Audit, entity: Entity)(implicit graph: Graph, authContext: AuthContext): Unit = {
    val createdAudit = auditSrv.create(audit)
    edgeSrv.create(Audited(), createdAudit, entity)
    ()
  }

  override def createVertex[V <: Product](graph: Graph, authContext: AuthContext, model: Model.Vertex[V], v: V): V with Entity = {
    val vEntity = db.createVertex(graph, authContext, model, v)
    create(Audit(AuditableAction.Creation, authContext.requestId, None, None, None), vEntity)(graph, authContext)
    vEntity
  }

  override def createEdge[E <: Product, FROM <: Product, TO <: Product](
      graph: Graph,
      authContext: AuthContext,
      model: Model.Edge[E, FROM, TO],
      e: E,
      from: FROM with Entity,
      to: TO with Entity): E with Entity = db.createEdge(graph, authContext, model, e, from, to)

  override def update(graph: Graph, authContext: AuthContext, model: Model, id: String, fields: Map[FPath, UpdateOps.Type]): Unit = {
    val element = model.get(id)(db, graph)
    val updateFields = fields.map {
      case (FPath("password"), _) if model.label == "User" ⇒ ("password", "**hidden**", "**hidden**")
      case (FPath(field), UpdateOps.SetAttribute(value)) ⇒
        val mapping  = model.fields(field)
        val oldValue = db.getProperty(element, field, mapping)
        (field, oldValue.toString, value.toString)
    }.toSeq

    db.update(graph, authContext, model, id, fields)

    updateFields.foreach {
      case (field, oldValue, newValue) ⇒
        create(
          Audit(AuditableAction.Update, authContext.requestId, Some(field), Some(oldValue), Some(newValue)),
          model.toDomain(element.asInstanceOf[model.ElementType]))(graph, authContext)
    }
  }

  override def update(
      graph: Graph,
      authContext: AuthContext,
      elementSrv: ElementSrv[_, _],
      id: String,
      properties: Seq[PublicProperty[_, _, _]],
      fields: Map[FPath, UpdateOps.Type]): Unit = {
    val element = elementSrv.get(id)(graph).asInstanceOf[ElementSteps[_, _, _]].raw.asInstanceOf[GremlinScala[Any]]

    val updateFields: Seq[(String, String, String)] = fields.map {
      case (FPath("password"), _) if elementSrv.model.label == "User" ⇒ ("password", "**hidden**", "**hidden**")
      case (FPath(field), UpdateOps.SetAttribute(value)) ⇒
        properties
          .find(_.propertyName == field)
          .flatMap { prop ⇒
            prop.get(Some(authContext)).headOption.asInstanceOf[Option[GremlinScala[Any] ⇒ GremlinScala[_]]]
          }
          .map(f ⇒ (field, f(element).toString, value.toString))
          .getOrElse(throw BadRequestError(s"Field $field not found"))
    }.toSeq

    db.update(graph, authContext, elementSrv, id, properties, fields)

    updateFields.foreach {
      case (field, oldValue, newValue) ⇒
        create(
          Audit(AuditableAction.Update, authContext.requestId, Some(field), Some(oldValue), Some(newValue)),
          elementSrv.model.toDomain(element.asInstanceOf[elementSrv.model.ElementType])
        )(graph, authContext)
    }
  }

  override def noTransaction[A](body: Graph ⇒ A): A                                                            = db.noTransaction(body)
  override def transaction[A](body: Graph ⇒ A): A                                                              = db.transaction(body)
  override def version: Int                                                                                    = db.version
  override def setVersion(v: Int): Unit                                                                        = db.setVersion(v)
  override def getModel[E <: Product: ru.TypeTag]: Base[E]                                                     = db.getModel[E]
  override def getVertexModel[E <: Product: ru.TypeTag]: Model.Vertex[E]                                       = db.getVertexModel[E]
  override def getEdgeModel[E <: Product: ru.TypeTag, FROM <: Product, TO <: Product]: Model.Edge[E, FROM, TO] = db.getEdgeModel[E, FROM, TO]
  override def createSchemaFrom(schemaObject: Schema)(implicit authContext: AuthContext): Unit                 = db.createSchemaFrom(schemaObject)(authContext)
  override def createSchema(model: Model, models: Model*): Unit                                                = db.createSchema(model, models: _*)
  override def createSchema(models: Seq[Model]): Unit                                                          = db.createSchema(models)
  override def drop(): Unit                                                                                    = db.drop()

  override def getSingleProperty[D, G](element: Element, key: String, mapping: SingleMapping[D, G]): D = db.getSingleProperty(element, key, mapping)
  override def getOptionProperty[D, G](element: Element, key: String, mapping: OptionMapping[D, G]): Option[D] =
    db.getOptionProperty(element, key, mapping)
  override def getListProperty[D, G](element: Element, key: String, mapping: ListMapping[D, G]): Seq[D] = db.getListProperty(element, key, mapping)
  override def getSetProperty[D, G](element: Element, key: String, mapping: SetMapping[D, G]): Set[D]   = db.getSetProperty(element, key, mapping)
  override def getProperty[D](element: Element, key: String, mapping: Mapping[D, _, _]): D              = db.getProperty(element, key, mapping)
  override def setSingleProperty[D, G](element: Element, key: String, value: D, mapping: SingleMapping[D, _]): Unit =
    db.setSingleProperty[D, G](element, key, value, mapping)
  override def setOptionProperty[D, G](element: Element, key: String, value: Option[D], mapping: OptionMapping[D, _]): Unit =
    db.setOptionProperty[D, G](element, key, value, mapping)
  override def setListProperty[D, G](element: Element, key: String, values: Seq[D], mapping: ListMapping[D, _]): Unit =
    db.setListProperty[D, G](element, key, values, mapping)
  override def setSetProperty[D, G](element: Element, key: String, values: Set[D], mapping: SetMapping[D, _]): Unit =
    db.setSetProperty[D, G](element, key, values, mapping)
  override def setProperty[D](element: Element, key: String, value: D, mapping: Mapping[D, _, _]): Unit = db.setProperty(element, key, value, mapping)
  override def vertexStep(graph: Graph, model: Model): GremlinScala[Vertex]                             = db.vertexStep(graph, model)
  override def edgeStep(graph: Graph, model: Model): GremlinScala[Edge]                                 = db.edgeStep(graph, model)
  //  override def loadBinary(id: String)(implicit graph: Graph): InputStream                               = db.loadBinary(id)(graph)
//  override def saveBinary(id: String, is: InputStream)(implicit graph: Graph): Vertex                   = db.saveBinary(id, is)(graph)
  override val extraModels: Seq[Model] = db.extraModels
}