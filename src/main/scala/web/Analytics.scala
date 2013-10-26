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
  var currentView: View = views.head
  var currentLayout: Layout = Layout(currentView.layout, None, Restrictions.empty)

  def setView(view: View) {
    currentView = view
    refreshLayout()
  }

  def refreshLayout() {
    currentLayout = Layout(currentView.layout, None, Restrictions.empty)
  }

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
    List(
      View("Channels", Left(rules.map(rule => TagLayout(rule, Some(Right(DimensionLayout(Dimension.source, None))))))),
      View("Source / medium", Right(DimensionLayout(Dimension.sourceMedium, None)))
    )
  }
}
