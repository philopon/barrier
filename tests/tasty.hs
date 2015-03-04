{-# LANGUAGE OverloadedStrings #-}
import Lens.Family
import Graphics.Badge.Barrier

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain $ testGroup ""
    [ testGroup "flat"
        [ goldenVsString "1" "tests/data/flat/1.svg" . return $ renderBadge flat "build" "success"
        , goldenVsString "2" "tests/data/flat/2.svg" . return $ renderBadge (flat & right .~ red) "build" "failing"
        , goldenVsString "3" "tests/data/flat/3.svg" . return $ renderBadge (flat & left .~ green) "leaf" "leaf"
        , goldenVsString "4" "tests/data/flat/4.svg" . return $ renderBadge flat "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"
        ]
    , testGroup "flatSquare"
        [ goldenVsString "1" "tests/data/flatSquare/1.svg" . return $ renderBadge flatSquare "build" "success"
        , goldenVsString "2" "tests/data/flatSquare/2.svg" . return $ renderBadge (flatSquare & right .~ red) "build" "failing"
        , goldenVsString "3" "tests/data/flatSquare/3.svg" . return $ renderBadge (flatSquare & left .~ green) "leaf" "leaf"
        , goldenVsString "4" "tests/data/flatSquare/4.svg" . return $ renderBadge flatSquare "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"
        ]
    , testGroup "plastic"
        [ goldenVsString "1" "tests/data/plastic/1.svg" . return $ renderBadge plastic "build" "success"
        , goldenVsString "2" "tests/data/plastic/2.svg" . return $ renderBadge (plastic & right .~ red) "build" "failing"
        , goldenVsString "3" "tests/data/plastic/3.svg" . return $ renderBadge (plastic & left .~ green) "leaf" "leaf"
        , goldenVsString "4" "tests/data/plastic/4.svg" . return $ renderBadge plastic "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"
        ]
    ]
