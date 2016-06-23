module Main exposing (..)

import ElmTest
import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import Claims.PrimaryIndex
import Claims.SecondarySetIndex
import Time


claims : Claim
claims =
    suite "Root"
        [ Claims.PrimaryIndex.claims
        , Claims.SecondarySetIndex.claims
        ]


main =
    ElmTest.runSuiteHtml (Check.Test.evidenceToTest (quickCheck claims))
