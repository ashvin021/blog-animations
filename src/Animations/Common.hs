{-# LANGUAGE OverloadedStrings #-}

module Animations.Common where

import Control.Lens ((%=), (&), (.=), (.~), (?~))
import qualified Data.Text as T
import qualified Graphics.SvgTree.Types as G
import Reanimate
import Reanimate.Scene (Origin)
import Text.Printf (printf)

-- Colour Themes

data AnimationTheme = DarkTheme | LightTheme

env :: AnimationTheme -> Animation -> Animation
env theme = mapA $ \svg ->
  mkGroup
    [ mkBackground bg,
      withFillOpacity 0 $
        withStrokeWidth 0.02 $
          withStrokeColor stroke (mkGroup [svg])
    ]
  where
    (bg, stroke) = getThemeColours theme

getThemeColours :: AnimationTheme -> (String, String)
getThemeColours DarkTheme = ("black", "white")
getThemeColours LightTheme = ("white", "black")

getThemePrimary = fst . getThemeColours

getThemeSecondary = snd . getThemeColours

withFillColorAndOpacity :: String -> Double -> SVG -> SVG
withFillColorAndOpacity s d = withFillOpacity d . withFillColor s

withThemePrimary :: AnimationTheme -> SVG -> SVG
withThemePrimary t = withFillColorAndOpacity (getThemePrimary t) 1

withThemeSecondary :: AnimationTheme -> SVG -> SVG
withThemeSecondary t = withFillColorAndOpacity (getThemeSecondary t) 1

-- Custom Shapes

mkRoundedRect :: Double -> Double -> Double -> SVG
mkRoundedRect width height cornerRadius =
  G.rectangleTree $
    G.defaultSvg
      & G.rectUpperLeftCorner .~ (G.Num (- width / 2), G.Num (- height / 2))
      & G.rectWidth ?~ G.Num width
      & G.rectHeight ?~ G.Num height
      & G.rectCornerRadius .~ (Just (G.Num cornerRadius), Just (G.Num cornerRadius))

mkCircleWithText :: Double -> T.Text -> AnimationTheme -> SVG
mkCircleWithText r txt theme
  = mkGroup [ mkCircle r
            , withThemeSecondary theme $ scale 0.75 $ center $ latexMono txt ]

mkCircleWithInt :: Double -> Int -> AnimationTheme -> SVG
mkCircleWithInt r i theme
  = mkGroup [ mkCircle r
            , withThemeSecondary theme $ scale 0.75 $ center $ latexMonoInt i ]

-- Custom Animations

oSlideInFromTop :: SVG -> Animation
oSlideInFromTop svg = oSlideInFrom (x + w / 2, screenTop + h / 2) svg
  where
    (x, _y, w, h) = boundingBox svg

oSlideInFrom :: Origin -> SVG -> Animation
oSlideInFrom = oSlideIn' (curveS 2)

oSlideIn' :: Signal -> Origin -> SVG -> Animation
oSlideIn' easing (originX, originY) svg =
  signalA easing $
    animate $ \t ->
      withGroupOpacity (t * 5) $
        translate (t * dx) (t * dy) $
          translate (- dx) (- dy) svg
  where
    (x, y, w, h) = boundingBox svg
    (dx, dy) = (x + (w / 2) - originX, y + (h / 2) - originY)

oSlideOutToBottom :: SVG -> Animation
oSlideOutToBottom svg = oSlideOutTo (x + w / 2, screenBottom - h / 2) svg
  where
    (x, _y, w, h) = boundingBox svg

oSlideOutTo :: Origin -> SVG -> Animation
oSlideOutTo origin = reverseA . oSlideIn' (curveS 2) origin

-- Custom Latex

latexMono :: T.Text -> SVG
latexMono t = latex $ T.pack $ printf "\\texttt{%s}" t

latexMonoInt :: Int -> SVG
latexMonoInt i = latex $ T.pack $ printf "\\texttt{%d}" i
