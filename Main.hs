module Main where
import Control.Monad ( forM_ )
import Data.Function ( on )
import Data.List ( minimumBy )
import Data.Maybe ( fromMaybe )
import Data.Word ( Word8 )
import System.Environment ( getArgs, getProgName )
import Codec.Picture.Types ( convertImage, promoteImage )
import Codec.Picture
  ( DynamicImage
      ( ImageY8
      , ImageYA8
      , ImageRGB8
      , ImageRGBA8
      , ImageYCbCr8
      , ImageCMYK8
      )
  , Image ( imageHeight, imageWidth )
  , PixelRGB8 ( PixelRGB8 )
  , PixelRGBA8 ( PixelRGBA8 )
  , pixelAt
  , readImage
  )

-- | Terminal color.
data TermColor = C216 !Word8 !Word8 !Word8 -- ^ 216-palette color.
                                           --   Range: [0, 6) each
               | CGry !Word8               -- ^ Grayscale color.
                                           --   Range: [0, 24)
               | CS16 !Word8               -- ^ Standard 16-palette color.
                                           --   Range: [0, 16)
               deriving Show

-- | List of the standard 16-palette colors.  Note that most terminals allow
--   these colors to be customized, so it's generally not accurate.
s16Colors :: [PixelRGB8]
s16Colors = [ PixelRGB8   0   0   0
            , PixelRGB8 128   0   0
            , PixelRGB8   0 128   0
            , PixelRGB8 128 128   0
            , PixelRGB8   0   0 128
            , PixelRGB8 128   0 128
            , PixelRGB8   0 128 128
            , PixelRGB8 192 192 192
            , PixelRGB8 128 128 128
            , PixelRGB8 255   0   0
            , PixelRGB8   0 255   0
            , PixelRGB8 255 255   0
            , PixelRGB8   0   0 255
            , PixelRGB8 255   0 255
            , PixelRGB8   0 255 255
            , PixelRGB8 255 255 255
            ]

-- | Measure the squared Euclidean distance between two colors in RGB space.
colorDist :: PixelRGB8 -> PixelRGB8 -> Int
PixelRGB8 r1 g1 b1 `colorDist` PixelRGB8 r2 g2 b2 = z
  where z  = sq (i r1 - i r2) + sq (i g1 - i g2) + sq (i b1 - i b2)
        i  = fromIntegral
        sq = (^ (2 :: Int))

-- | Convert alpha (transparency) into a character.
alphaToChar :: Word8 -> Char
alphaToChar x
  | x <  80   = ' '
  | otherwise = '#'

-- | Encode the terminal color as an 8-bit number.
encodeTermColor :: TermColor -> Word8
encodeTermColor (C216 r g b) = (r * 6 + g) * 6 + b + 16
encodeTermColor (CGry x) = 232 + x
encodeTermColor (CS16 x) = x

-- | Construct the escape code for a terminal color.
escapeTermColor :: Maybe TermColor -> ShowS
escapeTermColor Nothing  = showString "\ESC[0;00m"
escapeTermColor (Just c) = showString "\ESC[38;5;" .
                           shows (encodeTermColor c) .
                           showString "m"

-- | Approximate an RGB color as a 216-palette terminal color.
approx216 :: PixelRGB8 -> TermColor
approx216 (PixelRGB8 r g b) = C216 (approx r) (approx g) (approx b)
  where approx x
          | x <  48   = 0
          | x < 115   = 1
          | otherwise = (x - 35) `div` 40

-- | Approximate an RGB color as a grayscale terminal color.
approxGry :: PixelRGB8 -> TermColor
approxGry (PixelRGB8 r g b) = CGry . approx $ fromIntegral z
  where z = sum (fmap fromIntegral [r, g, b + 1]) `div` 3 :: Int
        approx x
          | x <   3   =  0
          | x < 233   = (x - 3) `div` 10
          | otherwise = 23

-- | Approximate an RGB color as a standard 16-palette terminal color.
approxS16 :: PixelRGB8 -> TermColor
approxS16 c = CS16 . fst . minimumBy (compare `on` snd) $
              zip [0 .. ] (colorDist c `fmap` s16Colors)

-- | Approximate an RGB color as a terminal color.
rgbToTermColor :: PixelRGB8 -> TermColor
rgbToTermColor c
  | d216 <= dGry = c216
  | otherwise    = cGry
  where c216 = approx216 c
        cGry = approxGry c
        d216 = c `colorDist` termColorToRGB c216
        dGry = c `colorDist` termColorToRGB cGry

-- | Convert a terminal color into an RGB color.
termColorToRGB :: TermColor -> PixelRGB8
termColorToRGB (CGry x) = PixelRGB8 y y y where y = x * 10 + 8
termColorToRGB (CS16 x) = s16Colors !! fromIntegral x
termColorToRGB (C216 r g b) = PixelRGB8 (convert r) (convert g) (convert b)
  where convert x = if x == 0 then 0 else x * 40 + 35

-- | Convert a @DynamicImage@ into an 8-bit RGBA image, if possible.
dynamicToRGBA8 :: DynamicImage -> Maybe (Image PixelRGBA8)
dynamicToRGBA8 dynImg =
  case dynImg of
    ImageY8     i -> Just $ promoteImage i
    ImageYA8    i -> Just $ promoteImage i
    ImageRGB8   i -> Just $ promoteImage i
    ImageRGBA8  i -> Just i
    ImageYCbCr8 i -> Just $ promoteImage (convertImage i :: Image PixelRGB8)
    ImageCMYK8  i -> Just $ promoteImage (convertImage i :: Image PixelRGB8)
    _             -> Nothing

-- | Convert a pixel into a colored text character.
pixelToChar :: PixelRGBA8 -> ShowS
pixelToChar (PixelRGBA8 r g b a) = escapeTermColor (Just termColor) .
                                   showChar (alphaToChar a)
  where termColor = convert $ PixelRGB8 r g b
        convert   = rgbToTermColor -- use either  rgbToTermColor  or  approxS16

-- | Main function.
main :: IO ()
main = do

  -- maximum width of the console; used to center the image
  let maxWidth = 80

  -- print usage message if no arguments are provided
  args <- getArgs
  name <- getProgName
  if null args then putStrLn $ "Usage: " ++ name ++ " FILE... " else return ()
  forM_ args $ \ arg -> do

    -- read the image file
    dynImg <- either error id `fmap` readImage arg
    let img    = error "unsupported format" `fromMaybe` dynamicToRGBA8 dynImg
        width  = imageWidth  img
        height = imageHeight img
        indent = replicate ((maxWidth - width) `div` 2) ' '

    -- loop over each pixel
    forM_ [0 .. height - 1] $ \ y -> do
      putStr indent
      forM_ [0 .. width - 1] $ \ x -> do
        putStr $ pixelToChar (pixelAt img x y) ""
      putStr "\n"
