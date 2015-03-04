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

import Graphics.Badge.Barrier.Style.Flat
import Graphics.Badge.Barrier.Style.FlatSquare
import Graphics.Badge.Barrier.Style.Plastic

import Text.Blaze.Svg.Renderer.Utf8

import Data.Text(Text)
import Data.ByteString.Lazy(ByteString)

-- | @
-- renderBadge flat "left" "right"
-- @
renderBadge :: Badge b => b
            -> Text -- ^ left text
            -> Text -- ^ right text
            -> ByteString -- ^ svg string
renderBadge b l r = renderSvg $ makeBadge b l r
