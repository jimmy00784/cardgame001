package controllers

import play.api.mvc._
import play.modules.reactivemongo.MongoController
import reactivemongo.api.collections.default._
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import play.modules.reactivemongo.json.BSONFormats._
import reactivemongo.bson.utils.Converters._
import Json.format
import reactivemongo.bson._
/**
 * Created by karim on 6/25/15.
 */
object Game extends Controller with MongoController {

  lazy val gameColl = db("games")

  def index = Action.async { implicit request =>
    val futActiveGames = gameColl.find(BSONDocument("active" -> true),BSONDocument("_id" -> 1)).cursor[BSONDocument].collect[List]()
    for {
      activeGames <- futActiveGames
    } yield {
      Ok(Json.toJson(activeGames.map{ _.getAs[String]("_id").get }))
    }
  }

  def newGame = Action.async { implicit request =>
    val id = BSONObjectID.generate.stringify
    gameColl.insert(BSONDocument("_id" -> id,"active" -> true,"started" -> false)) map {
      lastError =>
        if(lastError.inError)
          BadRequest
        else
          Ok(Json.obj("gameId" -> id))
    }
  }

  def invitePlayers(gameId:String) = Action {
    Ok
  }

  def joinGame(gameId:String) = Action.async {
    val playerId = BSONObjectID.generate.stringify
    gameColl.update(BSONDocument("_id" -> gameId,"active" -> true,"started" -> false),
      BSONDocument("$push" -> BSONDocument("players" -> BSONDocument("id" -> playerId,"cardsDown" -> List[Int](),"cardsUp" -> List[Int](),"cardsInHand" -> List[Int]())))
    ) map {
      lastError =>
        if(lastError.inError)
          BadRequest
        else
          Ok(Json.obj("playerId" -> playerId))
    }
  }

  def startGame(gameId:String) = Action.async {
    val deck = scala.util.Random.shuffle((1 to 52).map(c => c))

    gameColl.update(BSONDocument("_id" -> gameId,"active" -> true,"started" -> false,"players" -> BSONDocument("$size" -> 4)),
      BSONDocument("$set" -> BSONDocument("cards" -> deck,"pile" -> List[Int](),"direction" -> 1,"lucky" -> List[Int](),
        "players.0.cardsDown" -> (0 to 3).map{i => deck.toSeq(i)},"players.1.cardsDown" -> (4 to 7).map{i => deck.toSeq(i)},"players.2.cardsDown" -> (8 to 11).map{i => deck.toSeq(i)},"players.3.cardsDown" -> (12 to 16).map{i => deck.toSeq(i)},
        "players.0.cardsUp" -> (16 to 19).map{i => deck.toSeq(i)},"players.1.cardsUp" -> (20 to 23).map{i => deck.toSeq(i)},"players.2.cardsUp" -> (24 to 27).map{i => deck.toSeq(i)},"players.3.cardsUp" -> (28 to 31).map{i => deck.toSeq(i)},
        "players.0.cardsInHand" -> (32 to 36).map{i => deck.toSeq(i)},"players.1.cardsInHand" -> (37 to 41).map{i => deck.toSeq(i)},"players.2.cardsInHand" -> (42 to 46).map{i => deck.toSeq(i)},"players.3.cardsInHand" -> (47 to 51).map{i => deck.toSeq(i)},
      "currentPlayer" -> 1, "pileValue" -> 2, "started" -> true
      ))) map {
      lastError =>
        if (lastError.inError || lastError.n == 0)
          BadRequest
        else
          Ok("Started")
    }
  }

  def gameView(gameId:String) = Action.async {
    gameColl.find(BSONDocument("_id" -> gameId),BSONDocument("cards" -> 0, "players.cardsInHand" -> 0,"players.cardsDown" -> 0)).one.map {
      optGame =>
        optGame match {
          case Some(game) => Ok(Json.toJson(game))
          case None => Ok("No Game Found")
        }
    }
  }

  def playerView(gameId:String,playerId:String) = Action.async {
    gameColl.find(BSONDocument("_id" -> gameId),BSONDocument("players" -> BSONDocument( "$elemMatch" -> BSONDocument("id" -> playerId)),"cards" ->0, "players.cardsDown" -> 0)).one.map {
      optGame =>
        optGame match {
          case Some(game) => Ok(Json.toJson(game))
          case None => Ok("No Game Found")
        }
    }
  }

  def playCard(gameId:String,playerId:String,card:Int) = Action {
    Ok
  }

  def gameStats(gameId:String) = Action {
    Ok
  }
}
