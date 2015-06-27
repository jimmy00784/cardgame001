package controllers

import play.api._
import play.api.mvc._
import play.modules.reactivemongo.MongoController

object Application extends Controller with MongoController {

  def index = Action {
    Ok(views.html.index())
  }

  def game = Action {
    Ok
  }

  def player = Action {
    Ok
  }

}
