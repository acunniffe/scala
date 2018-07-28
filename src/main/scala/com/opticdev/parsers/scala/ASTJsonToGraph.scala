package com.opticdev.parsers.scala

import com.opticdev.parsers.AstGraph
import com.opticdev.parsers.graph.{AstType, BuilderPhase, CommonAstNode, GraphBuilder}
import play.api.libs.json._

object ASTJsonToGraph {

  implicit val language: String = "scala"

  private val reservedKeysSeq : Seq[String] = Seq("type", "pos")
  private val reservedKeys : Set[String] = reservedKeysSeq.toSet

  def buildGraphFromJson(jsObject: JsObject) : AstGraph = {
    val graphBuilder = new GraphBuilder[CommonAstNode]()
    createASTNode(jsObject, "root", 0, graphBuilder.rootPhase)
    graphBuilder.graph
  }

  private def createASTNode(json: JsObject, childType: String, index: Int, builderPhase: BuilderPhase[CommonAstNode], fromArray : Boolean = false) : CommonAstNode = {

    val ks = keyset(json)
    if (!isNode(ks)) return null

    val nodeFeatures = json.fields.filter(i=> reservedKeys.contains(i._1))

    val propertyFields = json.fields.filter({
      case (key, value) => ks.diff(reservedKeys).contains(key)
    })

    val typee = nodeFeatures.find(_._1 == "type").get._2.as[JsString].value
    val pos = nodeFeatures.find(_._1 == "pos").get._2.as[JsObject].value

    val start = pos("start").as[JsNumber].value.toInt
    val end = pos("end").as[JsNumber].value.toInt

    val otherFields = propertyFields.filterNot({
      case (key, value) => value.isInstanceOf[JsArray] || value.isInstanceOf[JsObject]
    })

    val node = typee match {
      case _ => CommonAstNode(AstType(typee, language), Range(start, end), JsObject(otherFields))
    }

    val childrenBuilderPhase = builderPhase.addChild(index, childType, node, fromArray)

    propertyFields.filter({
      case (key, value)=> value.isInstanceOf[JsArray]
    }).foreach({
      case (key, value) => {

        //array of objects
        if (value.as[JsArray].value.forall(_.isInstanceOf[JsObject])) {
          value.asInstanceOf[JsArray].value.zipWithIndex.foreach({ e =>
              val (element: JsObject, idx) = (e._1, e._2)
              createASTNode(element, key, idx, childrenBuilderPhase, true)
          })
        }
        //array of arrays
        else if (value.as[JsArray].value.forall(_.isInstanceOf[JsArray])) {
          val typedValue: Seq[Seq[JsObject]] = value.as[JsArray].value.map(_.as[JsArray].value).asInstanceOf[Seq[Seq[JsObject]]]

          typedValue.zipWithIndex.foreach {
            case (virtualSeq, index) => {

              val positions = virtualSeq.map(positionFromObject)
              val virtualParentRange = Range(positions.map(_.start).min, positions.map(_.end).max)

              val virtualNode = CommonAstNode(AstType("virtual_"+key, language), virtualParentRange, JsObject.empty)
              val virtualNodePhase = childrenBuilderPhase.addChild(index, key+"_virtual", virtualNode, true)
              virtualSeq.zipWithIndex.foreach {
                case (vChild, idx) => createASTNode(vChild, key+"_virtual_elem", idx, virtualNodePhase, true)
              }
            }
          }

        } else {
          throw new Exception(key + " does not contain a parsable AST Node")
        }
      }
    })


    propertyFields.filter({
      case (key, value)=> value.isInstanceOf[JsObject] && isNode(keyset(value.as[JsObject]))
    }).foreach({
      case (key, value) => createASTNode(value.asInstanceOf[JsObject], key, 0, childrenBuilderPhase, false)
    })

    node
  }

  private def keyset(json: JsObject) : Set[String] = json.fields.map(_._1).toSet

  private def isNode(ks: Set[String]) : Boolean = ks.intersect(reservedKeys) == reservedKeys

  def positionFromObject(jsObject: JsObject) : Range = {
    val pos = (jsObject \ "pos").get

    val start = pos("start").as[JsNumber].value.toInt
    val end = pos("end").as[JsNumber].value.toInt

    Range(start, end)
  }

}
