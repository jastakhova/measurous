package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import sun.nio.cs.StandardCharsets
import org.apache.commons.codec.CharEncoding

class Boot {
  def boot {
    LiftRules.addToPackages("web")

    LiftRules.early.append(_.setCharacterEncoding(CharEncoding.UTF_8))

    LiftRules.siteMapFailRedirectLocation = List("/")

    LiftRules.exceptionHandler.prepend {
      case (_, _, exception) => {
        exception.printStackTrace()
        RedirectResponse("/")
      }
    }

    LiftRules.ajaxStart = Full( () => LiftRules.jsArtifacts.show("ajax-spinner").cmd )
    LiftRules.ajaxEnd = Full( () => LiftRules.jsArtifacts.hide("ajax-spinner").cmd )
  }
}
