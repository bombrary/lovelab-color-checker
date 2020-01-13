module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Parser as P exposing (Parser)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Json.Encode as E
import Json.Decode as D
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Task

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type alias Model =
  { inputs: Colors
  , colors: Colors
  , status: LoadJsonStatus
  }

type alias Colors = Dict String String
type LoadJsonStatus
  = Idle
  | Succeed
  | Failed String
  | Opening


initialColors : List (String, String)
initialColors =
  [ ("bg", "#fff")
  , ("fg", "#000")
  , ("nav-bg", "#eee")
  , ("nav-fg", "#007aff")
  , ("nav-border", "#ccc")
  , ("nav-title-fg", "#000")
  , ("login-bg", "#007aff")
  , ("login-fg", "#fff")
  , ("register-bg", "#e8efff")
  , ("register-fg", "#007aff")
  , ("segctl-bg", "#ccc")
  , ("segctl-on-bg", "#fff")
  , ("donebtn-fg", "#fff")
  , ("donebtn-bg", "#34c759")
  , ("cell-border", "#ccc")
  , ("nav-fg-disabled", "#aaa")
  , ("cell-deadline", "#f68")
  , ("cell-remain24", "#f80")
  , ("delbtn-fg", "#fff")
  , ("delbtn-bg", "#ff3b30")
  , ("switch-bg", "#34c759")
  , ("switch", "#fff")
  , ("bar", "#007aff")
  , ("bar-max", "#ff9500")
  , ("bar-border", "#888")
  , ("barlabel-fg", "#888")
  ]


init : () -> (Model, Cmd Msg)
init _ =
  ( { inputs = Dict.empty
    , colors = Dict.fromList initialColors
    , status = Idle
    }
  , Cmd.none
  )

type Msg
  = Changed Id String
  | SaveRequested
  | OpenRequested
  | JsonSelected File
  | JsonLoadResult (Result String (Dict String String))

type alias Id = String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Changed id string ->
      let colors = Dict.insert id string model.colors
          inputs = Dict.insert id string model.inputs
      in
      case validColor string of
        Just _ ->
          ( { model | inputs = inputs
                    , colors = colors
            }
          , Cmd.none
          )

        Nothing ->
          ( { model | inputs = inputs }
          , Cmd.none
          )

    SaveRequested ->
      ( model
      , Download.string "colors.json" "application/json"
          <| E.encode 2 (E.dict identity E.string model.colors)
      )
    
    OpenRequested ->
      ( model
      , Select.file ["application/json"] JsonSelected
      )

    JsonSelected file ->
      let decode = D.decodeString (D.dict D.string)
          task = File.toString file
                  |> Task.andThen
                       (\string -> 
                          case decode string of
                            Ok dict ->
                              Task.succeed dict

                            Err jsonError ->
                              Task.fail (D.errorToString jsonError)
                        )
      in
      ( { model | status = Opening }
      , Task.attempt JsonLoadResult task
      )

    JsonLoadResult result ->
      case result of
        Err message ->
          ( { model | status = Failed message }
          , Cmd.none
          )

        Ok colors ->
          ( { model | status = Succeed
                    , inputs = Dict.empty
                    , colors = colors
            }
          , Cmd.none
          )




view : Model -> Html Msg
view model =
  div [ class "wrapper" ]
    [ viewInputs model
    , viewIphones model
    ]

viewInputs : Model -> Html Msg
viewInputs model =
  div [ class "inputs-area" ]
    [ div [ class "sticky-container" ]
        [ viewJsonIO model
        , viewColorInputs model
        ]
    ]

viewJsonIO : Model -> Html Msg
viewJsonIO model =
  div
    [ class "json-io"
    ]
    [ div
        [ class "json-io-inputs"
        ]
        [ button
            [ onClick SaveRequested
            ]
            [ text "Save"
            ]
        , button
            [ onClick OpenRequested
            ]
            [ text "Open"
            ]
        ]
    , div
        [ class "json-io-status"
        ]
        [ viewJsonStatus model.status
        ]
    ]

viewJsonStatus : LoadJsonStatus -> Html Msg
viewJsonStatus status =
  case status of
    Idle ->
      p []
        [ text "Status: Idle" ]

    Succeed ->
      p []
        [ text "Status: Loaded." ]

    Opening ->
      p []
        [ text "Status: Opening..." ]

    Failed message ->
      p []
        [ text ("Error: " ++ message) ]

viewColorInputs : Model -> Html Msg
viewColorInputs model =
  table [ class "color-inputs" ]
    (List.map (colorInput model)
             (Dict.toList model.colors
               |> List.map Tuple.first))

colorInput : Model -> String -> Html Msg
colorInput { inputs, colors } colorName =
  tr [ class "color-input" ]
    [ td [ class "color-name" ]
        [ text (colorName ++ ": ") ]
    , td [ class "color-sample" ]
        [ input
            [ type_ "color"
            , value (normalizeColor (color colorName colors))
            , onInput (Changed colorName)
            , style "background-color" (color colorName colors)
            ]
            []
        ]
    , td []
        [ input
            [ onInput (Changed colorName)
            , placeholder (String.toLower (color colorName colors))
            , value (Maybe.withDefault "" (Dict.get colorName inputs))
            ]
            []
        ]
    ] 



viewIphones : Model -> Html Msg
viewIphones model =
  div [ class "iphones-area" ]
    [ viewRegisterStart model
    , viewTodoList model
    , viewTodoDetail model
    , viewTodoEdit model
    , viewAddTask model
    , viewStatistics model
    ]

viewRegisterStart : Model -> Html Msg
viewRegisterStart { inputs, colors } =
  div
    [ class "iphone registerstart"
    , style "background-color" (color "bg" colors)
    , style "color" (color "fg" colors)
    ]
    [ viewNavigation colors []
    , div
        [ class "contents" ]
        [ img
            [ class "app-logo"
            , src "./img/logo.jpg"
            ]
            []
        , div
            [ class "id-and-password" ]
            [ input
                [ placeholder "ID"
                ]
                []
            , input
                [ placeholder "Password"
                ]
                []
            ]
        , button
            [ class "login-btn"
            , style "background-color" (color "login-bg" colors)
            , style "color" (color "login-fg" colors)
            ]
            [ text "ログイン"
            ]
        , button
            [ class "register-btn"
            , style "background-color" (color "register-bg" colors)
            , style "color" (color "register-fg" colors)
            ]
            [ text "新規登録"
            ]
        ]
    ]


viewTodoList : Model -> Html Msg
viewTodoList { inputs, colors } =
  div
    [ class "iphone todolist"
    , style "background-color" (color "bg" colors)
    , style "color" (color "fg" colors)
    ]
    [ viewNavigation colors
        [ span 
            [ class "info-icon"
            , style "border-color" (color "nav-fg" colors)
            , style "color" (color "nav-fg" colors)
            ]
            [ text "i" ]
        , div
            [ class "segmented-ctl"
            , style "background-color" (color "segctl-bg" colors)
            ]
            [ div
                [ class "ctl-elem-off"
                , style "background-color" "none"
                ]
                [ viewPerson colors
                ]
            , div
                [ class "ctl-elem-on"
                , style "background-color" (color "segctl-on-bg" colors)
                ]
                [ viewPerson colors
                ]
            ]
        , div [ class "plus-icon" ]
            [ viewPlus colors
            ]
        ]
    , div
        [ class "contents" ]
        [ viewTable colors
        ]
    ]

viewPerson : Colors -> Svg Msg
viewPerson colors =
  S.svg
      [ style "width" "15px"
      , style "height" "15px"
      , SA.viewBox "-5 -2.5 10 10"
      ]
      [ S.circle
          [ SA.r "2px"
          , SA.stroke "none"
          , SA.fill (color "nav-title-fg" colors)
          ]
          []
      , S.path
          [ SA.stroke "none"
          , SA.fill (color "nav-title-fg" colors)
          , SA.d "M4,6 A 4,2.5 180 0 0 -4,6"
          ]
          []
      ]

viewPlus : Colors -> Svg Msg
viewPlus colors =
  S.svg
      [ style "width" "15px"
      , style "height" "15px"
      , SA.viewBox "-5 -5 10 10"
      ]
      [ S.path
          [ SA.stroke (color "nav-fg" colors)
          , SA.strokeWidth "1.5"
          , SA.fill "none"
          , SA.d "M0,-5 L0,5"
          ]
          []
      , S.path
          [ SA.stroke (color "nav-fg" colors)
          , SA.strokeWidth "2"
          , SA.fill "none"
          , SA.d "M-5,0 L5,0" 
          ]
          []
      ]

viewTable : Colors -> Html Msg
viewTable colors =
  ul [ class "task-table" ]
    [ tableCellPassed "発表" "1日過ぎてます" colors
    , tableCellPassed "掃除" "9時間過ぎてます" colors
    , tableCellRemain24 "ゴミ出し" "あと11時間" colors
    , tableCellRemain24 "ゴミ出し" "あと11時間" colors
    , tableCellRemain24 "ゼミ資料集め" "あと1日" colors
    , tableCellWithDone "ゼミ資料集め" "あと1日" colors
    , tableCellUsual "ゼミ資料集め" "あと1日" colors
    , tableCellUsual "ゼミ資料集め" "あと1日" colors
    , tableCellUsual "ゼミ資料集め" "あと1日" colors
    ]

tableCellPassed : String -> String -> Colors -> Html Msg
tableCellPassed title timeDesc colors =
  li
    [ class "table-cell"
    , style "border-bottom-color" (color "cell-border" colors)
    ]
    [ span []
        [ text title ]
    , span
        [ class "time-passed"
        , style "color" (color "cell-deadline" colors)
        ]
        [ text timeDesc ]
    ]

tableCellRemain24 : String -> String -> Colors -> Html Msg
tableCellRemain24 title timeDesc colors =
  li
    [ class "table-cell"
    , style "border-bottom-color" (color "cell-border" colors)
    ]
    [ span []
        [ text title ]
    , span
        [ class "time-24"
        , style "color" (color "cell-remain24" colors)
        ]
        [ text timeDesc ]
    ]

tableCellUsual : String -> String -> Colors -> Html Msg
tableCellUsual title timeDesc colors =
  li
    [ class "table-cell"
    , style "border-bottom-color" (color "cell-border" colors)
    ]
    [ span []
        [ text title ]
    , span
        [ class "time-usual"
        , style "color" (color "fg" colors)
        ]
        [ text timeDesc ]
    ]

tableCellWithDone : String -> String -> Colors -> Html Msg
tableCellWithDone title timeDesc colors =
  li
    [ class "table-cell-with-done"
    , style "border-bottom-color" (color "cell-border" colors)
    ]
    [ span [ class "title-overflow" ]
        [ text title ]
    , span
        [ class "time-24"
        , style "color" (color "cell-remain24" colors)
        ]
        [ text timeDesc
        ]
    , span
        [ class "done-btn"
        , style "background-color" (color "donebtn-bg" colors)
        , style "color" (color "donebtn-fg" colors)
        ]
        [ text "Done" ]
    ]

viewTodoDetail : Model -> Html Msg
viewTodoDetail { inputs, colors } =
  div
    [ class "iphone tododetail"
    , style "background-color" (color "bg" colors)
    , style "color" (color "fg" colors)
    ]
    [  viewNavigation colors
        [ div
          [ class "back-btn"
          ]
          [ span
              [ class "less-than-symbol"
              , style "color" (color "nav-fg" colors)
              ]
              [ text "<"
              ]
          , span
              [ class "back-text"
              , style "color" (color "nav-fg" colors)
              ]
              [ text "Back"
              ]
          ]
        , div
            [ class "nav-title"
            , style "color" (color "nav-title-fg" colors)
            ]
            [ text "備品買い出し"
            ]
        , div
            [ class "edit-btn"
            , style "color" (color "nav-fg" colors)
            ]
            [ text "編集"
            ]
        ]
    , div
        [ class "content"
        ]
        [ div
            [ class "whoisdoing"
            ]
            [ text "担当者: Taro"
            ]
        , div
            [ class "deadlinedate"
            ]
            [ text "期限: 2020年1月11日 16時"
            ]
        , div
            [ class "comment"
            ]
            [ text "コメント: "
            ]
        , button
            [ class "done-btn"
            , style "background-color" (color "donebtn-bg" colors)
            , style "color" (color "donebtn-fg" colors)
            ]
            [ text "Done" ]
        ]
    ]

viewTodoEdit : Model -> Html Msg
viewTodoEdit { colors } =
  div
    [ class "iphone todoedit"
    ]
    [ viewNavigation colors
        [ div
            [ class "cancel-btn"
            , style "color" (color "nav-fg" colors)
            ]
            [ text "キャンセル"
            ]
        , div
            [ class "complete-btn"
            , style "color" (color "nav-fg-disabled" colors)
            ]
            [ text "完了"
            ]
        ]
    , div
        [ class "content"
        ]
        [ div
            [ class "task-info" ]
            [ viewTaskInfoItem "タスク名: " "備品買い出し" colors
            , viewTaskInfoItem "担当者: " "Taro" colors
            , viewTaskInfoItem "期限:" "01/11 16:26" colors
            , viewTaskInfoItem "コメント:" "" colors
            ]
        , button
            [ class "del-btn"
            , style "color" (color "delbtn-fg" colors)
            , style "background-color" (color "delbtn-bg" colors)
            ]
            [ text "タスクを削除"
            ]
        ]
    ]

viewTaskInfoItem : String -> String -> Colors -> Html Msg
viewTaskInfoItem title default colors =
  div
    [ class "task-info-item" 
    ]
    [ label []
        [ text title ]
    , input
        [ value default
        ]
        []
    ]

viewAddTask : Model -> Html Msg
viewAddTask { colors } =
  div
    [ class "iphone addtask"
    ]
    [ viewNavigation colors
        [ div
            [ class "cancel-btn"
            , style "color" (color "nav-fg" colors)
            ]
            [ text "キャンセル"
            ]
        , div
            [ class "complete-btn"
            , style "color" (color "nav-fg-disabled" colors)
            ]
            [ text "完了"
            ]
        ]
    , div
        [ class "content"
        ]
        [ input
            [ placeholder "仕事名を入力してください。" ]
            []
        , div
            [ class "whoisdoing"
            ]
            [ input
                [ placeholder "担当者"
                , disabled True
                ]
                []
            , div
                [ class "select-auto-person"
                ]
                [ span
                    [ class "switch"
                    , style "background-color" (color "switch-bg" colors)
                    ]
                    [ div
                        [ class "switch-content"
                        , style "background-color" (color "switch" colors)
                        ]
                        []
                    ]
                , span
                    [ class "switch-description" 
                    ]
                    [ text "担当者を自動で選ぶ"
                    ]
                ]
            ]
        , input
            [ placeholder "締め切り"
            ]
            []
        , input
            [ placeholder "コメント"
            ]
            []
        ]
    ]

viewStatistics : Model -> Html Msg
viewStatistics { colors } =
  div
    [ class "iphone statistics"
    ]
    [ viewNavigation colors
        [ div
          [ class "back-btn"
          ]
          [ span
              [ class "less-than-symbol"
              , style "color" (color "nav-fg" colors)
              ]
              [ text "<"
              ]
          , span
              [ class "back-text"
              , style "color" (color "nav-fg" colors)
              ]
              [ text "Back"
              ]
          ]
        , div
            [ class "nav-title"
            , style "color" (color "nav-title-fg" colors)
            ]
            [ text "タスク統計"
            ]
        , div
            [ class "edit-btn"
            , style "color" (color "nav-fg" colors)
            ]
            [ text "Doneリスト"
            ]
        ]
    , div
        [ class "content"
        ]
        [ div
            [ class "bar-chart"
            ]
            [ viewBarContainer 7 7 "Taro" colors
            , viewBarContainer 6 7 "Jiro" colors
            , viewBarContainer 4 7 "Saburo" colors
            , viewBarContainer 2 7 "Shiro" colors
            , viewBarContainer 2 7 "Goro" colors
            , div
                [ class "bar-border"
                , style "border-color" (color "bar-border" colors)
                ]
                []
            ]
        ]
    ]
  
viewBarContainer : Int -> Int -> String -> Colors -> Html Msg
viewBarContainer n maxN nameLabel colors =
  let percent = (toFloat n) / (toFloat maxN) * 100
      colorcode = if n == maxN then
                    (color "bar-max" colors)
                  else
                    (color "bar" colors)
  in
  div
    [ class "bar-container"
    , style "height" ((String.fromFloat percent) ++ "%") 
    ]
    [ div
        [ class "bar-value"
        , style "color" (color "barlabel-fg" colors)
        ]
        [ text (String.fromInt n) ]
    , div
        [ class "bar"
        , style "background-color" colorcode
        ]
        []
    , div
        [ class "bar-name"
        , style "color" (color "barlabel-fg" colors)
        ]
        [ text nameLabel
        ]
    ]


viewNavigation : Colors -> List (Html Msg) -> Html Msg
viewNavigation colors list =
    div
      [ class "navigation"
      , style "background-color" (color "nav-bg" colors)
      , style "border-bottom" ("1px solid " ++ (color "nav-border" colors))
      ]
      list


color : String -> Colors -> String
color id colors =
  case Dict.get id colors of
    Just val->
      val

    Nothing ->
      "#f0f"

validColor: String -> Maybe String
validColor string =
  let charList = String.toList string
  in
    case charList of
      [] ->
        Nothing

      ['#',x0,x1,x2] ->
        Maybe.map (\x -> String.fromList ('#'::x)) (validHex [x0,x1,x2])

      ['#',x0,x1,x2,x3,x4,x5] ->
        Maybe.map (\x -> String.fromList('#'::x)) (validHex [x0,x1,x2,x3,x4,x5])

      _ ->
        Nothing

validHex : List Char -> Maybe (List Char)
validHex charList =
  let string = String.fromList charList
  in 
    if String.all isHex string then
      Just charList
    else
      Nothing

isHex : Char -> Bool
isHex c =
  case Char.isDigit c of
    True -> True

    False ->
      case Char.toLower c of
        'a' -> True
        'b' -> True
        'c' -> True
        'd' -> True
        'e' -> True
        'f' -> True
        _ -> False
      
normalizeColor : String -> String
normalizeColor string =
  let charList = String.toList string
  in
  case charList of
    ['#',a,b,c] ->
      String.fromList ['#',a,a,b,b,c,c]

    _ ->
      string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

