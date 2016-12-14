module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = createModel
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- TYPES


type Choice
    = Rock
    | Paper
    | Scissors
    | Lizard
    | Spock


type GameResult
    = PlayerWins
    | ComputerWins
    | Tie


type alias Score =
    Int


type Model
    = PlayerTurn Score
    | ComputerTurn Score Choice
    | GameOver Score Choice Choice GameResult


type Msg
    = NoOp
    | ChoiceClicked Choice
    | RandomNumberReceived (Result Http.Error Int)
    | Reset



-- MODEL


createModel : ( Model, Cmd Msg )
createModel =
    let
        model =
            PlayerTurn 0
    in
        ( model, Cmd.none )



-- UPDATE


randomNumberUrl =
    "https://www.random.org/integers/?num=1&min=1&max=5&col=1&base=10&format=plain&rnd=new"


grabRandomNumber : Cmd Msg
grabRandomNumber =
    Http.get randomNumberUrl Json.Decode.int
        |> Http.send RandomNumberReceived


winConditions : List ( Choice, Choice )
winConditions =
    [ ( Rock, Scissors )
    , ( Rock, Lizard )
    , ( Paper, Rock )
    , ( Paper, Spock )
    , ( Scissors, Paper )
    , ( Scissors, Lizard )
    , ( Lizard, Paper )
    , ( Lizard, Spock )
    , ( Spock, Rock )
    , ( Spock, Scissors )
    ]


wins : Choice -> Choice -> Bool
wins a b =
    let
        match =
            (==) ( a, b )
    in
        List.any match winConditions


determineResult : Choice -> Choice -> GameResult
determineResult player computer =
    if player == computer then
        Tie
    else if wins player computer then
        PlayerWins
    else
        ComputerWins


intToChoice : Int -> Choice
intToChoice n =
    case n of
        1 ->
            Rock

        2 ->
            Paper

        3 ->
            Scissors

        4 ->
            Lizard

        5 ->
            Spock

        _ ->
            intToChoice <| n - (n - 5)


updateScore : Score -> GameResult -> Score
updateScore score result =
    case result of
        Tie ->
            score

        PlayerWins ->
            score + 1

        ComputerWins ->
            score - 1


updatePlayerChoice : Choice -> Model -> ( Model, Cmd Msg )
updatePlayerChoice choice model =
    case model of
        ComputerTurn _ _ ->
            model ! []

        PlayerTurn score ->
            ComputerTurn score choice ! [ grabRandomNumber ]

        GameOver score _ _ _ ->
            ComputerTurn score choice ! [ grabRandomNumber ]


updateComputerChoice : Int -> Model -> ( Model, Cmd Msg )
updateComputerChoice n model =
    case model of
        ComputerTurn score player ->
            let
                computer =
                    intToChoice n

                result =
                    determineResult player computer

                newScore =
                    updateScore score result
            in
                GameOver newScore player computer result ! []

        _ ->
            model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ChoiceClicked choice ->
            updatePlayerChoice choice model

        RandomNumberReceived (Ok n) ->
            updateComputerChoice n model

        RandomNumberReceived (Err _) ->
            model ! []

        Reset ->
            createModel



-- VIEW


choiceToString : Choice -> String
choiceToString choice =
    case choice of
        Rock ->
            "Rock"

        Paper ->
            "Paper"

        Scissors ->
            "Scissors"

        Lizard ->
            "Lizard"

        Spock ->
            "Spock"


resultToString : GameResult -> String
resultToString result =
    case result of
        Tie ->
            "It's a tie!"

        PlayerWins ->
            "Yay, you win!"

        ComputerWins ->
            "You lose :("


choiceToClass : Choice -> String
choiceToClass =
    String.toLower << choiceToString


divider : Html a
divider =
    hr [] []


showChoice : Choice -> Html a
showChoice choice =
    div [ class ("card " ++ choiceToClass choice) ] []


showResult : GameResult -> Html a
showResult result =
    p [] [ text <| resultToString result ]


showPlayerScore : Score -> Html a
showPlayerScore score =
    p [] [ text <| "Score: " ++ (toString score) ]


resetButton : Html Msg
resetButton =
    button [ onClick Reset, class "reset" ] [ text "Reset!" ]


choiceButton : Choice -> Html Msg
choiceButton choice =
    button
        [ class <| "choice " ++ (choiceToClass choice)
        , onClick (ChoiceClicked choice)
        ]
        [ text <| choiceToString choice ]


maybeRender : (a -> Html Msg) -> Maybe a -> Html Msg
maybeRender f a =
    Maybe.map f a
        |> Maybe.withDefault (div [] [])


gameView : Score -> Maybe Choice -> Maybe Choice -> Maybe GameResult -> Html Msg
gameView score playerChoice computerChoice gameResult =
    div
        []
        [ choiceButton Rock
        , choiceButton Paper
        , choiceButton Scissors
        , choiceButton Lizard
        , choiceButton Spock
        , divider
        , maybeRender showChoice playerChoice
        , maybeRender showChoice computerChoice
        , maybeRender showResult gameResult
        , showPlayerScore score
        , maybeRender (\_ -> resetButton) gameResult
        ]


view : Model -> Html Msg
view model =
    case model of
        PlayerTurn score ->
            div []
                [ gameView score Nothing Nothing Nothing
                , h3 [] [ text "Make your move!" ]
                ]

        ComputerTurn score player ->
            gameView score (Just player) Nothing Nothing

        GameOver score player computer result ->
            gameView score (Just player) (Just computer) (Just result)
