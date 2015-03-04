{-# LANGUAGE OverloadedStrings #-}
import Lens.Family
import Graphics.Badge.Barrier
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = L.putStrLn $ renderBadge (flat & right .~ red) "build" "failing"
