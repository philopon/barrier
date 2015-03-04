{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment

import Control.Monad
import Control.Applicative
import Control.Exception

import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Foreign.C

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Face(glyph, FT_Face)
import Graphics.Rendering.FreeType.Internal.Vector(FT_Vector(FT_Vector))
import Graphics.Rendering.FreeType.Internal.Library(FT_Library)
import Graphics.Rendering.FreeType.Internal.GlyphSlot(advance)
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes(FT_ULong)

import Data.Typeable

data FTError = FTError String CInt deriving(Show, Typeable)
instance Exception FTError

ft :: String -> IO CInt -> IO ()
ft n f = do
    e <- f
    unless (e == 0) $ throwIO (FTError n e)

withFreeType :: (FT_Library -> IO a) -> IO a
withFreeType = bracket bra ket
  where
    bra = alloca $ \p -> ft "Init_FreeType" (ft_Init_FreeType p) >> peek p
    ket = ft "Done_FreeType" . ft_Done_FreeType

withFontFace :: FilePath -> (FT_Face -> IO a) -> FT_Library -> IO a
withFontFace fn m lib = bracket bra ket m
  where
    bra = alloca $ \p -> withCString fn $ \f ->
        ft "New_Face" (ft_New_Face lib f 0 p) >> peek p
    ket = ft "Done_Face" . ft_Done_Face

firstChar :: FT_Face -> IO FT_ULong
firstChar face = fromIntegral <$> ft_Get_First_Char face nullPtr

nextChar :: FT_Face -> FT_ULong -> IO FT_ULong
nextChar face c = ft_Get_Next_Char face c nullPtr

forCharM_ :: FT_Face -> (FT_ULong -> IO ()) -> IO ()
forCharM_ face m = firstChar face >>= go
  where
    go 0 = return ()
    go i = m i >> nextChar face i >>= go


main :: IO ()
main = getArgs >>= \case
    [fontPath] -> withFreeType $ withFontFace fontPath $ \face -> do
        ft "Set_Pixel_Sizes" $ ft_Set_Pixel_Sizes face 0 11
        slot <- peek $ glyph face
        forCharM_ face $ \c -> do
            ft "Load_Char" $ ft_Load_Char face c 0
            FT_Vector x _ <- peek $ advance slot
            case quotRem x 64 of
                (s, 0) -> putStrLn $ show c ++ '\t': show s
                _      -> fail "rem font size"

    _ -> do
        p <- getProgName
        putStrLn $ "Usage: " ++ p ++ " FONT_FILE"
