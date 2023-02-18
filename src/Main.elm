module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode
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


{-| 設定
-}
setting : { rows : Int, cols : Int, bombs : Int }
setting =
    { rows = 20
    , cols = 20
    , bombs = 50
    }


{-| ランダム位置に爆弾を配置
-}
addRandomBomb : Cmd Msg
addRandomBomb =
    Random.map2
        (\i j -> ( i, j ))
        (Random.int 1 setting.rows)
        (Random.int 1 setting.cols)
        |> Random.generate SetBomb


{-| セルの状態

  - Safe = 爆弾なし(まわりの爆弾の数)
  - Bomb = 爆弾
  - Flag = 旗を立てている

-}
type CellStatus
    = Safe Int
    | Bomb
    | Flag


{-| 爆弾位置
-}
type alias Bombs =
    Set.Set Pos


{-| 位置
-}
type alias Pos =
    ( Int, Int )


{-| 開き済みのセル状態
-}
type alias Cells =
    Dict.Dict Pos CellStatus


{-| モデル
-}
type alias Model =
    { bombs : Bombs
    , cells : Cells
    }


{-| メッセージ

  - Open = 該当位置のセルを開く
  - SetFlag = 該当位置のセルに旗を立てる
  - SetBomb = 該当位置に爆弾をセットする

-}
type Msg
    = Open Pos
    | SetFlag Pos
    | SetBomb Pos


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        --| モデルの初期状態
        initModel =
            { bombs = Set.empty, cells = Dict.empty }

        --| 初期実行コマンド
        initCmd =
            addRandomBomb
    in
    ( initModel, initCmd )


view : Model -> Browser.Document Msg
view model =
    let
        table =
            List.range 1 setting.rows
                |> List.map
                    (\r ->
                        List.range 1 setting.cols
                            |> List.map
                                (\c ->
                                    let
                                        pos =
                                            ( r, c )

                                        currentCell =
                                            Dict.get pos model.cells

                                        cellText =
                                            Html.text <|
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
                                        [ cellText ]
                                )
                            |> Html.tr []
                    )
                |> Html.table
                    [ Attrs.style "border-collapse" "collapse"
                    ]

        bombText =
            String.fromInt (model.cells |> Dict.filter (\_ a -> a == Flag) |> Dict.size) ++ " / " ++ String.fromInt setting.bombs

        description =
            Html.dl []
                [ Html.dt [] [ Html.text "左クリック" ]
                , Html.dd [] [ Html.text "セルをひらく" ]
                , Html.dt [] [ Html.text "右クリック" ]
                , Html.dd [] [ Html.text "旗を立てる" ]
                , Html.dt [] [ Html.text "フラグ立てた数 / 爆弾数" ]
                , Html.dd [] [ Html.text bombText ]
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
                , description
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
            ( { model | cells = updateCells model.bombs pos model.cells }, Cmd.none )

        -- 爆弾をセット
        SetBomb pos ->
            let
                bombs =
                    Set.insert pos model.bombs

                cmd =
                    if Set.size bombs >= setting.bombs then
                        -- 規定数に到達
                        Cmd.none

                    else
                        -- さらに追加
                        addRandomBomb
            in
            ( { model | bombs = bombs }, cmd )


updateCells : Bombs -> Pos -> Cells -> Cells
updateCells bombs pos cells =
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
                |> List.filter (\( a, b ) -> 0 < a && a <= setting.rows && 0 < b && b <= setting.cols)

        -- 周囲の爆弾数
        countCell =
            arround
                |> List.filter (\p -> Set.member p bombs)
                |> List.length

        -- 開いた後のセル状態
        openCell =
            if Set.member pos bombs then
                Bomb

            else
                Safe countCell
    in
    case Dict.get pos cells of
        -- まだ空いてない場合
        Nothing ->
            -- まわりの爆弾が０個なら周りもあける
            if openCell == Safe 0 then
                arround
                    |> List.foldl
                        (\a b -> updateCells bombs a b)
                        (Dict.insert pos openCell cells)

            else
                Dict.insert pos openCell cells

        -- もう空いてるなら何もしない
        _ ->
            cells
