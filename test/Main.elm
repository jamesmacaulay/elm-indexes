module Main exposing (..)

import ElmTest
import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import Claims.UniqueIndex
import Time


claims : Claim
claims =
    Claims.UniqueIndex.claims


main =
    ElmTest.runSuiteHtml (Check.Test.evidenceToTest (quickCheck claims))
