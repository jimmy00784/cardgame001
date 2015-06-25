package controllers

import play.api._
import play.api.mvc._
import play.modules.reactivemongo.MongoController

object Application extends Controller with MongoController {

  def index = Action {
    Ok(views.html.index("Your new application is ready. Connected to: " + db.name ))
  }

}
