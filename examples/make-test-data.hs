{-# LANGUAGE OverloadedStrings #-}
import Lens.Family
import Graphics.Badge.Barrier
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    L.writeFile "flat/1.svg" $ renderBadge flat "build" "success"
    L.writeFile "flat/2.svg" $ renderBadge (flat & right .~ red) "build" "failing"
    L.writeFile "flat/3.svg" $ renderBadge (flat & left .~ green) "leaf" "leaf"
    L.writeFile "flat/4.svg" $ renderBadge flat "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"

    L.writeFile "flatSquare/1.svg" $ renderBadge flatSquare "build" "success"
    L.writeFile "flatSquare/2.svg" $ renderBadge (flatSquare & right .~ red) "build" "failing"
    L.writeFile "flatSquare/3.svg" $ renderBadge (flatSquare & left .~ green) "leaf" "leaf"
    L.writeFile "flatSquare/4.svg" $ renderBadge flatSquare "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"

    L.writeFile "plastic/1.svg" $ renderBadge plastic "build" "success"
    L.writeFile "plastic/2.svg" $ renderBadge (plastic & right .~ red) "build" "failing"
    L.writeFile "plastic/3.svg" $ renderBadge (plastic & left .~ green) "leaf" "leaf"
    L.writeFile "plastic/4.svg" $ renderBadge plastic "longlonglonglonglonglonglonglonglonglonglonglong" "longlonglonglonglonglonglonglonglonglonglonglong"
