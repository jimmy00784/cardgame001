gameApp = angular.module "gameApp",['ngRoute']


gameApp.controller "GameController",['$scope','$http',($scope,$http) ->
    $scope.games = []
    $scope.currentGame = {}
    $scope.getGames = ()->
        $http.get("/game/").success (data) ->
            $scope.games = data
    $scope.newGame = ()->
        $http.get("/game/new").success ->
            $scope.getGames()
    $scope.getGame = (gameId) ->
        $http.get("/game/" + gameId).success (data) ->
            $scope.currentGame = data
    $scope.getCardPic = (card) ->
        suit = Math.floor(card / 13)
        suitname = "spades"
        switch suit
            when 1 then suitname = "hearts"
            when 2 then suitname = "clubs"
            else suitname = "diamonds"

        score = card % 13
        #console.log suit + " " + score
        facevalue = suitname
        switch score
            when 1 then facevalue = facevalue + "-" + "a" + "-150.png"
            when 11 then facevalue = facevalue + "-" + "j" + "-150.png"
            when 12 then facevalue = facevalue + "-" + "q" + "-150.png"
            when 0 then facevalue = facevalue + "-" + "k" + "-150.png"
            else facevalue = facevalue + "-" + score + "-150.png"
        facevalue
    $scope.playCard = (playerId,cardIdx) ->
        $http.get("/game/"+$scope.currentGame._id+"/play/"+playerId+"/"+cardIdx).success ->
            $scope.getGame($scope.currentGame._id)
    true]