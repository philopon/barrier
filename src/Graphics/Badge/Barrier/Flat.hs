{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Badge.Barrier.Flat
    ( Flat(..)
    , flat
    ) where

import Graphics.Badge.Barrier.Internal
import Graphics.Badge.Barrier.Color

import Text.Blaze.Svg11(Svg, AttributeValue, toValue, (!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

data Flat = Flat
    { leftColor  :: Color
    , rightColor :: Color
    }

instance Badge Flat where
    badge Flat{..} = flat' leftColor rightColor

instance HasLeftColor Flat where
    left f (Flat l r) = fmap (\l' -> Flat l' r) (f l)

instance HasRightColor Flat where
    right f (Flat l r) = fmap (\r' -> Flat l r') (f r)

flat :: Flat
flat = Flat gray brightgreen

flat' :: Color -> Color -> BadgeConfig -> Svg
flat' (Color colorA) (Color colorB) BadgeConfig{..} = S.docTypeSvg ! A.width (toValue svgWidth) ! A.height "20" $ do
    S.lineargradient ! A.id_ "smooth" ! A.x2 "0" ! A.y2 "100%" $ do
        S.stop ! A.offset "0" ! A.stopColor "#bbb" ! A.stopOpacity ".1"
        S.stop ! A.offset "1" ! A.stopOpacity ".1"

    S.mask ! A.id_ "round" $
        S.rect ! A.width (toValue svgWidth) ! A.height "20" ! A.rx "3" ! A.fill "#fff"

    S.g ! A.mask "url(#round)" $ do
        S.rect ! A.width (toValue widthLeft) ! A.height "20" ! A.fill (toValue colorA)
        S.rect ! A.x (toValue widthLeft) ! A.width (toValue widthRight) ! A.height "20" ! A.fill (toValue colorB)
        S.rect ! A.width (toValue svgWidth) ! A.height "20" ! A.fill "url(#smooth)"

    S.g ! A.fill "#fff" ! A.textAnchor "middle" ! A.fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! A.fontSize "11" $ do
        S.text_ (S.text textLeft)  ! A.x (float $ fromIntegral widthLeft / 2 + 1) ! A.y "15" ! A.fill "#010101" ! A.fillOpacity ".3"
        S.text_ (S.text textLeft)  ! A.x (float $ fromIntegral widthLeft / 2 + 1) ! A.y "14"
        S.text_ (S.text textRight) ! A.x (float $ fromIntegral widthLeft + fromIntegral widthRight / 2 - 1) ! A.y "15" ! A.fill "#010101" ! A.fillOpacity ".3"
        S.text_ (S.text textRight) ! A.x (float $ fromIntegral widthLeft + fromIntegral widthRight / 2 - 1) ! A.y "14"
        
  where
    svgWidth = widthLeft + widthRight
    float :: Double -> AttributeValue
    float = toValue
