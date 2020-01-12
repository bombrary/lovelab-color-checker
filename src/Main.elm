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
  }

type alias Colors = Dict String String


initialColors : List (String, String)
initialColors =
  [ ("bg", "#fff")
  , ("fg", "#000")
  , ("nav-bg", "#eee")
  , ("nav-fg", "#007AFF")
  , ("nav-border", "#ccc")
  , ("nav-title-fg", "#000")
  , ("login-bg", "#007AFF")
  , ("login-fg", "#fff")
  , ("register-bg", "#E8EFFF")
  , ("register-fg", "#007AFF")
  , ("segctl-bg", "#ccc")
  , ("segctl-on-bg", "#fff")
  , ("donebtn-fg", "#fff")
  , ("donebtn-bg", "#34C759")
  , ("cell-border", "#ccc")
  , ("nav-fg-disabled", "#aaa")
  , ("cell-deadline", "#f68")
  , ("cell-remain24", "#f80")
  , ("delbtn-fg", "#fff")
  , ("delbtn-bg", "#FF3B30")
  , ("switch-bg", "#34c759")
  , ("switch", "#fff")
  , ("bar", "#007aff")
  , ("bar-max", "#FF9500")
  , ("bar-border", "#888")
  , ("barlabel-fg", "#888")
  ]


init : () -> (Model, Cmd Msg)
init _ =
  ( { inputs = Dict.fromList initialColors
    , colors = Dict.fromList initialColors
    }
  , Cmd.none
  )

type Msg
  = Changed Id String

type alias Id = String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Changed id string ->
      let colors = Dict.insert id string model.colors
      in
      case validColor string of
        Just _ ->
          ( { inputs = colors
            , colors = colors
            }
          , Cmd.none
          )

        Nothing ->
          ( { model | inputs = colors }
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
    [ viewColorInputs model
    ]

viewColorInputs : Model -> Html Msg
viewColorInputs model =
  table [ class "color-inputs" ]
    (List.map (colorInput model)
             (Dict.toList model.colors
               |> List.map Tuple.first))

colorInput : Model -> String -> Html Msg
colorInput { inputs, colors } string =
  tr [ class "color-input" ]
    [ td [ class "color-name" ]
        [ text (string ++ ": ") ]
    , td [ class "color-sample" ]
        [ span 
            [ style "background-color" (color string colors)
            ]
            []
        ]
    , td []
        [ input
          [ onInput (Changed string)
          , placeholder (String.toLower (color string colors))
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

