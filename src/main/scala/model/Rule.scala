package model

import scala._
import model.TagLayout
import model.Rule
import model.DimensionLayout
import model.TagLayout
import model.Rule

/**
 * Created with IntelliJ IDEA.
 * User: Julia
 * Date: 10/26/13
 */
case class Rule(name: String, dimension: Dimension, pattern: String) {

  // Rule is applied to the row of the dimensions and adds its own dimensions to the row
  def apply(row : List[String]): List[String] = row
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
