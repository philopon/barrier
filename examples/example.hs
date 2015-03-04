{-# LANGUAGE OverloadedStrings #-}
import Lens.Family
import Graphics.Badge.Barrier
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    L.writeFile "passing.svg" $ renderBadge flat "build" "passing"
    L.writeFile "failing.svg" $ renderBadge (flat & right .~ red) "build" "failing"
