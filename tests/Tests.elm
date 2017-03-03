module Tests exposing (..)

import Test exposing (..)
import PatternTests
import DateParserTests
import InternalDateTests


all : Test
all =
    describe "Date Parser Test Suite"
        [ --DateParserTests.tests
          PatternTests.tests
        , InternalDateTests.tests
        ]
