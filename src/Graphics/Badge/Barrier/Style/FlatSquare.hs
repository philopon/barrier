{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Badge.Barrier.Style.FlatSquare
    ( FlatSquare(..)
    , flatSquare
    ) where

import Graphics.Badge.Barrier.Internal
import Graphics.Badge.Barrier.Color

import Text.Blaze.Svg11(Svg, AttributeValue, toValue, (!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

data FlatSquare = FlatSquare
    { leftColor  :: Color
    , rightColor :: Color
    }

instance Badge FlatSquare where
    badge FlatSquare{..} = flatSquare' leftColor rightColor

instance HasLeftColor FlatSquare where
    left f (FlatSquare l r) = fmap (\l' -> FlatSquare l' r) (f l)

instance HasRightColor FlatSquare where
    right f (FlatSquare l r) = fmap (\r' -> FlatSquare l r') (f r)

flatSquare :: FlatSquare
flatSquare = FlatSquare gray brightgreen

flatSquare' :: Color -> Color -> BadgeConfig -> Svg
flatSquare' (Color colorA) (Color colorB) BadgeConfig{..} = S.docTypeSvg ! A.width (toValue svgWidth) ! A.height "20" $ do
    S.g ! A.shapeRendering "crispEdges" $ do
        S.rect ! A.width (toValue widthLeft) ! A.height "20" ! A.fill (toValue colorA)
        S.rect ! A.x (toValue widthLeft) ! A.width (toValue widthRight) ! A.height "20" ! A.fill (toValue colorB)

    S.g ! A.fill "#fff" ! A.textAnchor "middle" ! A.fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! A.fontSize "11" $ do
        S.text_ (S.text textLeft)  ! A.x (float $ fromIntegral widthLeft / 2 + 1) ! A.y "14"
        S.text_ (S.text textRight) ! A.x (float $ fromIntegral widthLeft + fromIntegral widthRight / 2 - 1) ! A.y "14"
        
  where
    svgWidth = widthLeft + widthRight
    float :: Double -> AttributeValue
    float = toValue
