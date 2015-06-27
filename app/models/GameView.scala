package models

import play.api.libs.json.Json
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

/**
 * Created by karim on 6/26/15.
 */
case class GameViewPlayer(id:String,cardsDownCount:Int,cardsInHandCount:Int,cardsUp:List[Int])
case class GameView(
  _id:String,active:Boolean,started:Boolean,pile:List[Int],direction:Int,lucky:List[String],currentPlayer:Int,pileValue:Int, players:List[GameViewPlayer]
)

object GameView {
  implicit val formatGameViewPlayer = Json.format[GameViewPlayer]
  implicit val formatGameView = Json.format[GameView]

  implicit object GameViewPlayerReader extends BSONDocumentReader[GameViewPlayer] {
    def read(bson: BSONDocument) = GameViewPlayer(
      bson.getAs[String]("id").getOrElse(""),
      bson.getAs[List[Int]]("cardsDown").getOrElse(List[Int]()).length,
      bson.getAs[List[Int]]("cardsInHand").getOrElse(List[Int]()).length,
      bson.getAs[List[Int]]("cardsUp").getOrElse(List[Int]())
    )
  }

  implicit object GameViewReader extends BSONDocumentReader[GameView] {
    def read(bson: BSONDocument) = GameView(
      bson.getAs[String]("_id").getOrElse(""),
      bson.getAs[Boolean]("active").getOrElse(false),
      bson.getAs[Boolean]("started").getOrElse(false),
      bson.getAs[List[Int]]("pile").getOrElse(List[Int]()),
      bson.getAs[Int]("direction").getOrElse(1),
      bson.getAs[List[String]]("lucky").getOrElse(List[String]()),
      bson.getAs[Int]("currentPlayer").getOrElse(0),
      bson.getAs[Int]("pileValue").getOrElse(2),
      bson.getAs[List[GameViewPlayer]]("players").getOrElse(List[GameViewPlayer]())
    )
  }

}