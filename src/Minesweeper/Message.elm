module Minesweeper.Message exposing (..)

{-| メッセージ

  - Open = 該当位置のセルを開く
  - SetFlag = 該当位置のセルに旗を立てる
  - SetBomb = 該当位置に爆弾をセットする

-}

import Minesweeper.Model exposing (Pos, Setting)


type Msg
    = Open Pos
    | SetFlag Pos
    | SetBomb Pos
    | InputForm Setting
    | Reset
