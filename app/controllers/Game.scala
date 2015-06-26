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

import scala.concurrent.Future

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
      "currentPlayer" -> 0, "pileValue" -> 2, "started" -> true
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

  def playCard(gameId:String,playerId:String,card:Int) = Action.async {
    // Verify Player's turn
    val futUpdate:Future[Option[BSONDocument]] = gameColl.find(BSONDocument("_id" -> gameId,"active"-> true,"started" -> true)).one.map {
      case Some(game) => {
        val players = game.getAs[BSONArray]("players").getOrElse(BSONArray())

        val currentPlayer = game.getAs[Int]("currentPlayer").getOrElse(-1)

        val optPlayer = players.getAs[BSONDocument](currentPlayer)
        optPlayer match {
          case Some(player) => {
            if(playerId == player.getAs[String]("id").getOrElse("")) {
              val cardsInHand = player.getAs[List[Int]]("cardsInHand").getOrElse(List[Int]())
              val cardsUp = player.getAs[List[Int]]("cardsUp").getOrElse(List[Int]())
              val cardsDown = player.getAs[List[Int]]("cardsDown").getOrElse(List[Int]())
              val pile = game.getAs[List[Int]]("pile").getOrElse(List[Int]())
              val pileValue = game.getAs[Int]("pileValue").getOrElse(2)
              val direction = game.getAs[Int]("direction").getOrElse(1)

              trait CardPlayed
              case class CardsInHand(score:Int) extends CardPlayed
              case class CardsUp(score:Int) extends CardPlayed
              case class CardsDown(score:Int) extends CardPlayed
              case class NoCardPlayed() extends CardPlayed

              val playedCard = {
                if (cardsInHand.length > 0) {
                  if (card < cardsInHand.length)
                    CardsInHand(cardsInHand(card))
                  else
                    NoCardPlayed()
                }
                else if (cardsUp.length > 0) {
                  if (card < cardsUp.length)
                    CardsUp(cardsUp(card))
                  else
                    NoCardPlayed()
                }
                else if (cardsDown.length > 0) {
                  if (card < cardsDown.length)
                    CardsDown(cardsDown(card))
                  else
                    NoCardPlayed()
                }
                else
                  NoCardPlayed()
              }

              val score = playedCard match {
                case CardsInHand(score) => score
                case CardsUp(score) => score
                case CardsDown(score) => score
                case _ => 0
              }

              trait Outcome
              case class Normal() extends Outcome
              case class Reset() extends Outcome
              case class Skip() extends Outcome
              case class Reverse() extends Outcome
              case class Destroy() extends Outcome
              case class BadLuck() extends Outcome

              val faceValue = if((score % 13) == 0)  13 else (score % 13)
              val outcome = {
                if(faceValue == 2)
                  Reset()
                else if (faceValue == 4)
                  Skip()
                else if (faceValue == 7)
                  Reverse()
                else if (faceValue == 10)
                  Destroy()
                else if (faceValue == 1 || (faceValue >= pileValue && pileValue != 1))
                  Normal()
                else
                  BadLuck()
              }

              val pullCard = if(cardsDown.length + cardsUp.length + cardsInHand.length > 1) playedCard match {
                case CardsInHand(score) => BSONDocument("$pull" -> BSONDocument("players.$.cardsInHand" -> score))
                case CardsUp(score) => BSONDocument("$pull" -> BSONDocument("players.$.cardsUp" -> score))
                case CardsDown(score) => BSONDocument("$pull" -> BSONDocument("players.$.cardsDown" -> score))
              } else BSONDocument("$unset" -> BSONDocument("players." + currentPlayer -> 1),"$push" -> BSONDocument("lucky" -> playerId))
              val updateDocument = {
                outcome match {
                  case Normal() => pullCard ++ BSONDocument(
                    "$set" -> BSONDocument("pileValue" -> faceValue, "currentPlayer" -> {
                      if(currentPlayer + direction  >= 0)
                        (currentPlayer + direction) % players.length
                      else
                        currentPlayer + direction + players.length
                    }),
                    "$push" -> BSONDocument("pile" -> score)
                  )
                  case Reset() => pullCard ++ BSONDocument(
                    "$set" -> BSONDocument("pileValue" -> 2, "currentPlayer" -> {
                      if(currentPlayer + direction  >= 0)
                        (currentPlayer + direction) % players.length
                      else
                        currentPlayer + direction + players.length
                    }),
                    "$push" -> BSONDocument("pile" -> score)
                  )
                  case Skip() => pullCard ++ BSONDocument(
                    "$set" -> BSONDocument("currentPlayer" -> {
                      if(currentPlayer + (direction * 2) >= 0)
                        (currentPlayer + (direction * 2)) % players.length
                      else
                        currentPlayer + (direction * 2) + players.length
                    }),
                    "$push" -> BSONDocument("pile" -> score)
                  )
                  case Reverse() => pullCard ++ BSONDocument(
                    "$set" -> BSONDocument("currentPlayer" -> {
                      if(currentPlayer - direction  >= 0)
                        (currentPlayer - direction) % players.length
                      else
                        currentPlayer - direction + players.length
                    }, "direction" -> -direction),
                    "$push" -> BSONDocument("pile" -> score)
                  )
                  case Destroy() => pullCard ++ BSONDocument(
                    "$set" -> BSONDocument("pileValue" -> 2, "currentPlayer" -> {
                      if(currentPlayer + direction  >= 0)
                        (currentPlayer + direction) % players.length
                      else
                        currentPlayer + direction + players.length
                    }, "pile" -> List[Int]())
                  )
                  case BadLuck() => BSONDocument(
                    "$set" -> BSONDocument("pileValue" -> 2,"pile" -> List[Int]()),
                    "$push" -> BSONDocument("players.$.cardsInHand" -> BSONDocument("$each" -> pile))
                  ) ++ {playedCard match {
                    case CardsUp(score) => pullCard
                    case CardsDown(score) => pullCard
                    case _ => BSONDocument()
                  }}
                }
              }



              Some(updateDocument)
            }
            else {
              None
            }
          }
          case None => None
        }
      }
      case None => None
    }

    for {
      optUpdateDoc <- futUpdate
      lastError <- gameColl.update(BSONDocument("_id" -> gameId,"players.id" -> playerId),optUpdateDoc.get) if optUpdateDoc.isDefined
    } yield {
      gameColl.update(BSONDocument(),BSONDocument("$pull" -> BSONDocument("players" -> BSONDocument("$type" -> 10))))
      if(lastError.inError)
        BadRequest
      else
        Ok(Json.obj("status" -> "Move Accepted"))
    }


    // Play Turn and determine outcome
    // update pile, pileValue, currentPlayer, direction as necessary
  }

  def gameStats(gameId:String) = Action {
    Ok
  }
}
