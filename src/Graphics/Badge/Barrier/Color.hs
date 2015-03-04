{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Badge.Barrier.Color where

import Data.Text(Text)
import Data.String(IsString)

newtype Color = Color Text
    deriving(Show, Eq, IsString)

-- | #4c1
brightgreen :: Color
brightgreen = "#4c1"

-- | #97CA00
green :: Color
green = "#97CA00"

-- | #dfb317
yellow :: Color
yellow = "#dfb317"

-- | #a4a61d
yellowgreen :: Color
yellowgreen = "#a4a61d"

-- | #fe7d37
orange :: Color
orange = "#fe7d37"

-- | #e05d44
red :: Color
red = "#e05d44" 

-- | #007ec6
blue :: Color
blue = "#007ec6" 

-- | #555
gray :: Color
gray = "#555"

-- | #9f9f9f
lightgray :: Color
lightgray = "#9f9f9f"
