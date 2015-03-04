{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Badge.Barrier.Plastic
    ( Plastic(..)
    , plastic
    ) where

import Graphics.Badge.Barrier.Internal
import Graphics.Badge.Barrier.Color

import Text.Blaze.Svg11(Svg, AttributeValue, toValue, (!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

data Plastic = Plastic
    { leftColor  :: Color
    , rightColor :: Color
    }

instance Badge Plastic where
    badge Plastic{..} = plastic' leftColor rightColor

instance HasLeftColor Plastic where
    left f (Plastic l r) = fmap (\l' -> Plastic l' r) (f l)

instance HasRightColor Plastic where
    right f (Plastic l r) = fmap (\r' -> Plastic l r') (f r)

plastic :: Plastic
plastic = Plastic gray brightgreen

plastic' :: Color -> Color -> BadgeConfig -> Svg
plastic' (Color colorA) (Color colorB) BadgeConfig{..} = S.docTypeSvg ! A.width (toValue svgWidth) ! A.height "18" $ do
    S.lineargradient ! A.id_ "smooth" ! A.x2 "0" ! A.y2 "100%" $ do
        S.stop ! A.offset "0"   ! A.stopColor "#fff" ! A.stopOpacity ".7"
        S.stop ! A.offset "0.1" ! A.stopColor "#aaa" ! A.stopOpacity ".1"
        S.stop ! A.offset "0.9" ! A.stopColor "#000" ! A.stopOpacity ".3"
        S.stop ! A.offset "1"   ! A.stopColor "#000" ! A.stopOpacity ".5"

    S.mask ! A.id_ "round" $
        S.rect ! A.width (toValue svgWidth) ! A.height "18" ! A.rx "4" ! A.fill "#fff"

    S.g ! A.mask "url(#round)" $ do
        S.rect ! A.width (toValue widthLeft) ! A.height "18" ! A.fill (toValue colorA)
        S.rect ! A.x (toValue widthLeft) ! A.width (toValue widthRight) ! A.height "18" ! A.fill (toValue colorB)
        S.rect ! A.width (toValue svgWidth) ! A.height "18" ! A.fill "url(#smooth)"

    S.g ! A.fill "#fff" ! A.textAnchor "middle" ! A.fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! A.fontSize "11" $ do
        S.text_ (S.text textLeft)  ! A.x (float $ fromIntegral widthLeft / 2 + 1) ! A.y "14" ! A.fill "#010101" ! A.fillOpacity ".3"
        S.text_ (S.text textLeft)  ! A.x (float $ fromIntegral widthLeft / 2 + 1) ! A.y "13"
        S.text_ (S.text textRight) ! A.x (float $ fromIntegral widthLeft + fromIntegral widthRight / 2 - 1) ! A.y "14" ! A.fill "#010101" ! A.fillOpacity ".3"
        S.text_ (S.text textRight) ! A.x (float $ fromIntegral widthLeft + fromIntegral widthRight / 2 - 1) ! A.y "13"
        
  where
    svgWidth = widthLeft + widthRight
    float :: Double -> AttributeValue
    float = toValue
