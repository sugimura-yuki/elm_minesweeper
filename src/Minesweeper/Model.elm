module Minesweeper.Model exposing (..)

import Dict
import Set


type alias Setting =
    { rows : Int, cols : Int, bombs : Int }


type alias Model =
    { bombs : Bombs
    , cells : Cells
    , setting : Setting
    , settingForm : Setting
    }


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
