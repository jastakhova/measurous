package web

import model._
import model.Rule
import model.View

/**
 * Created with IntelliJ IDEA.
 * User: Julia
 * Date: 10/26/13
 */
object AnalyticsData {

  val rules: List[Rule] = createChannelRules
  val views: List[View] = createViews(rules)
  var currentLayout: Layout = Layout(views.head.layout, None)

  def createChannelRules: List[Rule] =
    List(
      Rule("SEO", Dimension.medium, "organic"),
      Rule("Direct", Dimension.source, "direct"),
      Rule("Social", Dimension.source, "facebook.com"),
      Rule("Social", Dimension.source, "twitter.com"),
      Rule("Social", Dimension.source, "t.co"),
      Rule("Social", Dimension.source, "plus.google.com"),
      Rule("Social", Dimension.source, "instagram.com"),
      Rule("Social", Dimension.source, "pinterest.com"),
      Rule("Paid search", Dimension.medium, "CPC"),
      Rule("Paid search", Dimension.medium, "PPC"),
      Rule("Display", Dimension.network, "content"),
      Rule("Display", Dimension.medium, "cpm"),
      Rule("Display", Dimension.medium, "display"),
      Rule("Email", Dimension.medium, "email")
    )

  def createViews(rules: List[Rule]): List[View] = {
    List(View("Channels", Left(rules.map(rule => TagLayout(rule, None)))))
  }
}
