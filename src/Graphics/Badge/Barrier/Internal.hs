{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Graphics.Badge.Barrier.Internal
    ( BadgeConfig(..)
    , Badge(..)
    , makeBadge
    , Lens'
    
    , HasLeftColor(..)
    , HasRightColor(..)
    ) where

import Language.Haskell.TH

import Control.Arrow(second, (***))

import Text.Blaze.Svg11(Svg)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as S

import Graphics.Badge.Barrier.Color

advanceDict :: S.HashMap Char Int
advanceDict = $( do
    dat <- runIO $ readFile "data/advance.txt"
    let convI = litE . integerL . read
        convC = litE . charL . toEnum . read
    let kvs = flip map (lines dat) $ tupE . (\(a,b) -> [a,b]) .
            (convC *** convI) . second tail . break (== '\t')
    [|S.fromList $(listE kvs)|]
    )

advance :: Char -> Int
advance c = S.lookupDefault 11 c advanceDict

measureText :: T.Text -> Int
measureText = T.foldl' (\i c -> advance c + i) 0

data BadgeConfig = BadgeConfig
    { textLeft   :: T.Text
    , textRight  :: T.Text
    , widthLeft  :: Int
    , widthRight :: Int
    }

badge' :: T.Text -> T.Text -> BadgeConfig
badge' l r = BadgeConfig l r (measureText l + 10) (measureText r + 10)

makeBadge :: Badge b => b -> T.Text -> T.Text -> Svg
makeBadge b l r = badge b (badge' l r)

class Badge a where
    badge :: a -> BadgeConfig -> Svg

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

class HasLeftColor a where
    left :: Lens' a Color

class HasRightColor a where
    right :: Lens' a Color
