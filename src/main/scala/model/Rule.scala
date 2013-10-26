package model

import scala._
import model._
import com.google.api.services.analytics.Analytics
import web.AnalyticsData
import com.google.api.services.analytics.model.GaData

import scala.collection.JavaConversions._
import model.TagLayout
import model.Layout
import model.DimensionLayout
import model.Rule
import scala.Some


/**
 * Created with IntelliJ IDEA.
 * User: Julia
 * Date: 10/26/13
 */
case class Rule(name: String, dimension: Dimension, pattern: String) {

  // Rule is applied to the row of the dimensions and adds its own dimensions to the row
  def apply(row : Map[Dimension, String]): Option[String] =
    row.get(dimension).filter(_.contains(pattern)).map(_ => name)
}

object Dimension {
  val medium = Dimension("medium")
  val source = Dimension("source")
  val network = Dimension("adDistributionNetwork")

  def fromHeader(name: String) = Dimension(name.replaceAll("^ga:", ""))
}

case class Dimension(name: String) {
  def getGAName = "ga:" + name.toLowerCase

  def getUIName = name.capitalize
}

case class TagLayout(rule: Rule, inner: Option[Either[List[TagLayout], DimensionLayout]])

case class DimensionLayout(val dimension: Dimension, inner: Option[Either[List[TagLayout], DimensionLayout]])

case class View(name: String, layout: Either[List[TagLayout], DimensionLayout])

case class Layout(layout: Either[List[TagLayout], DimensionLayout], parent: Option[Layout]) {
  def moveInto(tagLayout: TagLayout): Option[Layout] = tagLayout.inner.map(layout => Layout(layout, Some(this)))

  def moveInto(dimensionLayout: DimensionLayout): Option[Layout] =
    dimensionLayout.inner.map(layout => Layout(layout, Some(this)))

  def getName = layout match {
    case Left(_) => "Tags"
    case Right(dimentionLayout) => dimentionLayout.dimension.getUIName
  }
}

object EnrichedData {

  def fromRaw(data: GaData) = EnrichedData(
    data.getColumnHeaders.filter(_.getColumnType == "METRIC").map(header => Dimension.fromHeader(header.getName)).toList,
    data.getRows.map(row => EnrichedRow.fromRaw(data.getColumnHeaders.map(_.getName).toList, row.toList)).toList
  )
}

case class EnrichedData(headers: List[Dimension], data: List[EnrichedRow])

object EnrichedRow {
  def fromRaw(headers: List[String], data: List[String]) = {
    val mappedData = headers.zip(data).foldLeft(Map[Dimension, String]())
      {case (res, (header, data)) => res.updated(Dimension.fromHeader(header), data)}
    EnrichedRow(mappedData, AnalyticsData.rules.flatMap(_.apply(mappedData)).toSet.toList)
  }
}

case class EnrichedRow(data: Map[Dimension, String], tags: List[String])