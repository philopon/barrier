module Graphics.Badge.Barrier
    ( -- * badge
      Badge
    , flat
    , flatSquare
    , plastic

    -- * render function
    , renderBadge

      -- * lens
    , HasLeftColor(..)
    , HasRightColor(..)

    , module Graphics.Badge.Barrier.Color
    ) where

import Graphics.Badge.Barrier.Internal
import Graphics.Badge.Barrier.Color

import Graphics.Badge.Barrier.Flat
import Graphics.Badge.Barrier.FlatSquare
import Graphics.Badge.Barrier.Plastic

import Text.Blaze.Svg.Renderer.Utf8

import Data.Text(Text)
import Data.ByteString.Lazy(ByteString)

renderBadge :: Badge b => b
            -> Text -- ^ left text
            -> Text -- ^ right text
            -> ByteString -- ^ svg string
renderBadge b l r = renderSvg $ makeBadge b l r
