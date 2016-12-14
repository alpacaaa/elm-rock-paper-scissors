module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App


all : Test
all =
    describe "A Test Suite"
        [ test "intToChoice should not crash with numbers greater than 5" <|
            \() ->
                Expect.equal (App.intToChoice (12)) Main.Msg.Rock
        ]
