port module Main exposing (Msg(..), init, main, setStorage, update, updateWithStorage, view, viewChoice, viewChoices, viewControls, viewControlsCount, viewControlsReset, viewEntry, viewQuizNavigation)

import Array
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Process exposing (sleep)
import Tuple

main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Angle Quiz", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ setStorage newModel, cmds ] )


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel, Cmd.none )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | Reset
    | NextEntry
    | PreviousEntry
    | SelectAndNext Int String



-- How we update our Model on a given Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | uid = defaultID
                , current = 0
                , entries = defaultEntries
              }
            , Cmd.none
            )

        NextEntry ->
            if model.current < List.length model.entries then
                ( model |> nextEntry, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousEntry ->
            if model.current > 0 then
                ( model |> previousEntry, Cmd.none )

            else
                ( model, Cmd.none )

        SelectAndNext selectedId id ->
            ( model
                |> selectAnswer selectedId id
                |> nextEntry
            , Cmd.none
            )


-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewEntry model
            , viewSummary model.entries model.current
            , viewControls model.entries model.current
            ]
        ]


viewEntry : Model -> Html Msg
viewEntry model =
    let
        examArr =
            Array.fromList model.entries

        entry =
            case Array.get model.current examArr of
                Just ent ->
                    ent

                Nothing ->
                    newEntry "" [] -1 model.uid

        title =
            entry.description

        choices =
            entry.answers

        imagePath = 
            "images/" ++ entry.uid ++ ".png"

        isCorrect entryInput =
            entryInput.selected == entryInput.correct

        correctCnt =
            List.length (List.filter isCorrect model.entries)

        totalCnt =
            List.length model.entries

        hiddenFlag =
            if totalCnt > 0 && model.current == totalCnt then
                "none"

            else
                "block"
    in
    div []
        [ header
            [ class "header" ]
            [ h1 [] [ text "Angle Quiz" ]
            , p [ class "new-todo", style "display" hiddenFlag ] [ text title ]
            , img [src imagePath, width 250, height 250, class "question-image"] []
            , div [ class "reminder" ] [text "Click the circles to select that answer!" ]
            ]
        , viewChoices choices model.uid model.current entry
        ]


viewChoices : List String -> String -> Int -> Entry -> Html Msg
viewChoices answerChoices uid current entry =
    let
        viewKeyedChoice : ( Int, String ) -> ( String, Html Msg )
        viewKeyedChoice indexDesc =
            ( Tuple.second indexDesc, viewChoice indexDesc uid current entry )
    in
    section
        [ class "main" ]
        [ Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedChoice (List.indexedMap Tuple.pair answerChoices)
        ]


viewChoice : ( Int, String ) -> String -> Int -> Entry -> Html Msg
viewChoice indexDesc uid current entry =
    let
        answerIndex =
            Tuple.first indexDesc

        questionText =
            Tuple.second indexDesc

        -- "id" FORMAT for exam "exam-alpha", for each question "exam-alpha-0"
        questionId =
            uid ++ "-" ++ String.fromInt current

        isCorrect =
            entry.selected == entry.correct && entry.correct == answerIndex

        isIncorrect =
            entry.selected /= entry.correct && entry.selected == answerIndex

        isChecked =
            entry.selected == answerIndex
    in
    li
        [ classList [ ( "entry-correct", isCorrect ), ( "entry-incorrect", isIncorrect ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ classList [ ( "toggle", True ), ( "toggle-checked", isChecked ) ]
                , type_ "checkbox"
                , onClick (SelectAndNext answerIndex questionId)
                ]
                []
            , label
                []
                [ text questionText ]
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewSummary : List Entry -> Int -> Html Msg
viewSummary entries current =
    let
        isCorrect entry =
            entry.selected == entry.correct

        isSelected entry =
            entry.selected /= -1

        correctCnt =
            List.length (List.filter isCorrect entries)

        totalCnt =
            List.length entries

        entriesLeft =
            totalCnt - List.length (List.filter isSelected entries)

        {--hidden/show-}
        hiddenFlag =
            if totalCnt > 0 && current == totalCnt then
                "block"

            else
                "none"

        examScore =
            String.fromInt correctCnt
                ++ "/"
                ++ String.fromInt totalCnt
                ++ " : Grade : "
                ++ String.fromFloat ((toFloat correctCnt / toFloat totalCnt) * 100)
                ++ "%"
    in
    div
        [ class "header"
        , style "display" hiddenFlag
        ]
        [ section
            [ class "summary" ]
            [ h2 [] [ text "Quiz Summary" ]
            , text examScore
            ]
        ]


viewControls : List Entry -> Int -> Html Msg
viewControls entries current =
    let
        isCorrect entry =
            entry.selected == entry.correct

        isSelected entry =
            entry.selected /= -1

        correctCnt =
            List.length (List.filter isCorrect entries)

        totalCnt =
            List.length entries

        entriesLeft =
            totalCnt - List.length (List.filter isSelected entries)
    in
    footer
        [ class "footer", hidden (List.isEmpty entries) ]
        [ lazy3 viewControlsCount correctCnt totalCnt entriesLeft
        , lazy viewQuizNavigation current
        , viewControlsReset
        ]


viewControlsCount : Int -> Int -> Int -> Html Msg
viewControlsCount correctCnt totalCnt entriesLeft =
    let
        examScore =
            --String.fromInt correctCnt ++ "/" ++ String.fromInt totalCnt ++ " "
            " "

        examStatus =
            if totalCnt > 0 && entriesLeft == 0 then
                --"Grade : " ++ String.fromFloat ((toFloat correctCnt / toFloat totalCnt) * 100) ++ "%"
                "Completed"

            else if entriesLeft == 1 then
                String.fromInt entriesLeft ++ " question left"

            else
                String.fromInt entriesLeft ++ " questions left"
    in
    span
        [ class "todo-count" ]
        [ text examScore
        , strong [] [ text examStatus ]
        ]


viewQuizNavigation : Int -> Html Msg
viewQuizNavigation currentIndex =
    ul
        [ class "filters" ]
        [ li
            [ onClick PreviousEntry ]
            [ img [ class "elm-quiz-btn-prev" ] [] ]
        , text " "
        , text (" | " ++ String.fromInt (currentIndex + 1) ++ " | ")
        , text " "
        , li
            [ onClick NextEntry ]
            [ img [ class "elm-quiz-btn-next" ] [] ]
        ]


viewControlsReset : Html Msg
viewControlsReset =
    span [ class "clear-completed" ]
        [ button
            [ onClick Reset ]
            [ text "Restart" ]
        ]

type alias Model =
    { entries : List Entry
    , current : Int
    , uid : String
    , error : String
    }


type alias Entry =
    { description : String
    , answers : List String
    , selected : Int
    , correct : Int
    , uid : String
    }


type alias Entries =
    List Entry


nextEntry : Model -> Model
nextEntry model =
    { model | current = model.current + 1 }


previousEntry : Model -> Model
previousEntry model =
    { model | current = model.current - 1 }


selectAnswer : Int -> String -> Model -> Model
selectAnswer selectedId uid model =
    let
        updateEntry entry =
            if entry.uid == uid then
                { entry | selected = selectedId }

            else
                entry
    in
    { model | entries = List.map updateEntry model.entries }


emptyModel : Model
emptyModel =
    { entries =
        defaultEntries
    , current = 0
    , uid = defaultID
    , error = ""
    }


newEntry : String -> List String -> Int -> String -> Entry
newEntry desc answers correct uid =
    { description = desc
    , answers = answers
    , selected = -1
    , correct = correct
    , uid = uid
    }


defaultID =
    "default"

defaultEntries =
    [ 
        newEntry "How many right angles are in this pentagon?" [ "0", "1", "2", "5" ] 0 "default-0",
        newEntry "What is the angle of x?" [ "47°", "54°", "57°", "82°" ] 0 "default-1",
        newEntry "What is the angle of y?" [ "45°", "30°", "60°", "90°" ] 1 "default-2",
        newEntry "What is the angle of x?" [ "125°", "65°", "55°", "90°" ] 2 "default-3",
        newEntry "What is the angle of x?" [ "150°", "130°", "120°", "140°" ] 3 "default-4",
        newEntry "What is the angle of z?" [ "55°", "65°", "40°", "115°" ] 0 "default-5",
        newEntry "What is the angle of x?" [ "45°", "150°", "70°", "60°" ] 2 "default-6",
        newEntry "What is the angle of X?" [ "50°", "95°", "65°", "35°" ] 0 "default-7",
        newEntry "What is the angle of z?" [ "115°", "75°", "50°", "80°" ] 3 "default-8",
        newEntry "What is the angle of x?" [ "50°", "40°", "60°", "90°" ] 1 "default-9"
    ]
