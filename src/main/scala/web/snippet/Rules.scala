package web.snippet

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
class Rules {

  def rulesNodes = <span/>

  def render = {
    "@rules" #> rulesNodes
  }
}
