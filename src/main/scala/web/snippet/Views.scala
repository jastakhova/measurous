package web.snippet

import web.AnalyticsData
import model.{DimensionLayout, TagLayout}
import xml.NodeSeq

import net.liftweb.http._
import js.JE.ValById
import js.JsCmds.SetHtml
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import js.{JsCmd, JsCmds}

/**
 * Created with IntelliJ IDEA.
 * User: Julia
 * Date: 10/26/13
 */
class Views {

  def toNode(tagLayout: TagLayout, indent: Int): NodeSeq = {
    <button class="btn layoutBtn btn-warning" type="button" style={"margin-left:" + 20*indent + "px"}>
      {tagLayout.rule.name}</button><button class="rightNeibour btn btn-mini" type="button"><i class="icon-remove"></i>
    </button><br/><span>{tagLayout.inner.map(_ match {
      case Left(x) => <span>{x.groupBy(_.rule.name).map(_._2.head).map(toNode(_, indent + 1))}</span>
      case Right(x) => toNode(x, indent + 1)
    }).getOrElse(<span/>)}</span>
  }

  def toNode(dimenstionLayout: DimensionLayout, indent: Int): NodeSeq = {
    <button class="btn layoutBtn" type="button" style={"margin-left:" + 20*indent + "px"}>
      {dimenstionLayout.dimension.getUIName}</button><button class="rightNeibour btn btn-mini" type="button"><i class="icon-remove"></i>
    </button><br/><span>{dimenstionLayout.inner.map(_ match {
      case Left(x) => <span>{x.groupBy(_.rule.name).map(_._2.head).map(toNode(_, indent + 1))}</span>
      case Right(x) => toNode(x, indent + 1)
    }).getOrElse(<span/>)}</span>
  }

  def viewsNodes = AnalyticsData.views.map(view => {
    <h3>{view.name}</h3><span>{
    view.layout match {
      case Left(x) => <span>{x.groupBy(_.rule.name).map(_._2.head).map(toNode(_, 0))}</span>
      case Right(x) => toNode(x, 0)
    }}</span>
  })

  def render = {
     "@views" #> viewsNodes
  }
}
