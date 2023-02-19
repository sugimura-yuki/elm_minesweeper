module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode
import Minesweeper.Message exposing (Msg(..))
import Minesweeper.Model exposing (CellStatus(..), Model, Pos, Setting)
import Random
import Set


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


{-| ランダム位置に爆弾を配置
-}
randomBomb : Setting -> Cmd Msg
randomBomb setting =
    let
        randomPos =
            Random.map2
                (\i j -> ( i, j ))
                (Random.int 1 setting.rows)
                (Random.int 1 setting.cols)
    in
    Random.generate SetBomb randomPos


init : flags -> ( Model, Cmd Msg )
init _ =
    reset
        { rows = 20
        , cols = 20
        , bombs = 50
        }


reset : Setting -> ( Model, Cmd Msg )
reset setting =
    let
        --| モデルの初期状態
        initModel =
            { bombs = Set.empty
            , cells = Dict.empty
            , setting = setting
            , settingForm = setting
            }

        --| 初期実行コマンド
        initCmd =
            randomBomb initModel.setting
    in
    ( initModel, initCmd )


view : Model -> Browser.Document Msg
view model =
    let
        cell ( r, c ) =
            let
                pos =
                    ( r, c )

                currentCell =
                    Dict.get pos model.cells

                cellText =
                    case currentCell of
                        Just Flag ->
                            "🏁"

                        Just Bomb ->
                            "💣"

                        Just (Safe 0) ->
                            ""

                        Just (Safe n) ->
                            String.fromInt n

                        _ ->
                            ""
            in
            Html.td
                [ Attrs.style "width" "30px"
                , Attrs.style "height" "30px"
                , Attrs.style "border" "1px solid black"
                , Attrs.style "user-select" "none"
                , Attrs.style "text-align" "center"
                , Attrs.style "cursor" <|
                    if currentCell == Nothing then
                        "pointer"

                    else
                        "auto"
                , Attrs.style "background-color" <|
                    if currentCell == Nothing then
                        "#aaa"

                    else
                        "#fff"
                , Events.preventDefaultOn "contextmenu" (Json.Decode.succeed ( SetFlag pos, True ))
                , Events.onClick (Open pos)
                ]
                [ Html.text cellText ]

        table =
            List.range 1 model.setting.rows
                |> List.map
                    (\r ->
                        List.range 1 model.setting.cols
                            |> List.map (\c -> cell ( r, c ))
                            |> Html.tr []
                    )
                |> Html.table
                    [ Attrs.style "border-collapse" "collapse"
                    ]

        bombText =
            String.fromInt (model.cells |> Dict.filter (\_ a -> a == Flag) |> Dict.size) ++ " / " ++ String.fromInt model.setting.bombs

        description =
            Html.dl []
                [ Html.dt [] [ Html.text "左クリック" ]
                , Html.dd [] [ Html.text "セルをひらく" ]
                , Html.dt [] [ Html.text "右クリック" ]
                , Html.dd [] [ Html.text "旗を立てる" ]
                , Html.dt [] [ Html.text "フラグ立てた数 / 爆弾数" ]
                , Html.dd [] [ Html.text bombText ]
                ]

        form =
            let
                settingForm =
                    model.settingForm

                makeInputForm labelText value updateSetting =
                    Html.label
                        [ Attrs.style "display" "block"
                        ]
                        [ Html.dl []
                            [ Html.dt [] [ Html.text labelText ]
                            , Html.dd []
                                [ Html.input
                                    [ Attrs.type_ "number"
                                    , Attrs.value (value |> String.fromInt)
                                    , Events.onInput
                                        (\val ->
                                            case String.toInt val of
                                                Nothing ->
                                                    InputForm settingForm

                                                Just n ->
                                                    InputForm (updateSetting n)
                                        )
                                    ]
                                    []
                                ]
                            ]
                        ]
            in
            Html.div []
                [ makeInputForm "縦のサイズ" settingForm.rows (\n -> { settingForm | rows = n })
                , makeInputForm "横のサイズ" settingForm.cols (\n -> { settingForm | cols = n })
                , makeInputForm "爆弾の数" settingForm.bombs (\n -> { settingForm | bombs = n })
                , Html.button [ Events.onClick Reset ] [ Html.text "再作成" ]
                ]
    in
    { title = "マインスイーパー"
    , body =
        [ Html.main_ []
            [ Html.div
                [ Attrs.style "display" "flex"
                , Attrs.style "flex-wrap" "wrap"
                , Attrs.style "gap" "10px"
                ]
                [ table
                , Html.div []
                    [ description
                    , form
                    ]
                ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- 旗を立てる
        SetFlag pos ->
            let
                cells =
                    model.cells
                        |> Dict.update pos
                            (\v ->
                                case v of
                                    Nothing ->
                                        Just Flag

                                    Just Flag ->
                                        Nothing

                                    _ ->
                                        v
                            )
            in
            ( { model | cells = cells }, Cmd.none )

        -- セルを開く
        Open pos ->
            ( openCell pos model, Cmd.none )

        -- 爆弾をセット
        SetBomb pos ->
            let
                bombs =
                    Set.insert pos model.bombs

                cmd =
                    if Set.size bombs < model.setting.bombs then
                        randomBomb model.setting

                    else
                        Cmd.none
            in
            ( { model | bombs = bombs }, cmd )

        InputForm form ->
            ( { model | settingForm = form }, Cmd.none )

        Reset ->
            reset model.settingForm


openCell : Pos -> Model -> Model
openCell pos model =
    let
        ( r, c ) =
            pos

        -- 周囲のセル
        arround =
            [ ( r, c - 1 )
            , ( r, c + 1 )
            , ( r - 1, c - 1 )
            , ( r - 1, c )
            , ( r - 1, c + 1 )
            , ( r + 1, c - 1 )
            , ( r + 1, c )
            , ( r + 1, c + 1 )
            ]
                |> List.filter (\( a, b ) -> 0 < a && a <= model.setting.rows && 0 < b && b <= model.setting.cols)

        -- 周囲の爆弾数
        countCell =
            arround
                |> List.filter (\p -> Set.member p model.bombs)
                |> List.length

        -- 開いた後のセル状態
        cellStatus =
            if Set.member pos model.bombs then
                Bomb

            else
                Safe countCell

        updatedModel =
            { model | cells = Dict.insert pos cellStatus model.cells }
    in
    case Dict.get pos model.cells of
        -- まだ空いてない場合
        Nothing ->
            -- まわりの爆弾が０個なら周りもあける
            if cellStatus == Safe 0 then
                arround
                    |> List.foldl
                        openCell
                        updatedModel

            else
                updatedModel

        -- もう空いてるなら何もしない
        _ ->
            model
