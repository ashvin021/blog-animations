{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, DuplicateRecordFields #-}

module Animations.Haskell.BasicFunctions where

import Animations.Common
import Control.Lens ((%=), (.~))
import Control.Monad (forM_, void)
import qualified Data.Text as T
import Linear.V2
import Reanimate
import Reanimate.LaTeX (latexCfg, noto)
import Reanimate.Scene
import Graphics.SvgTree
  ( Origin (OriginAbsolute),
    PathCommand (MoveTo, SmoothCurveTo),
    RPoint,
  )

data UnaryFn s = UnaryFn { input   :: Object s SVG,
                           output  :: Object s SVG,
                           outline :: Object s SVG,
                           fnType  :: Object s SVG
                         }

data BinaryFn s = BinaryFn { input1  :: Object s SVG
                           , input2  :: Object s SVG
                           , output  :: Object s SVG
                           , outline :: Object s SVG
                           , fnType  :: Object s SVG
                           }

--------------------------------------------------------------------------------

basicFunctions :: Animation
basicFunctions = basicFunctions' DarkTheme

basicFunctionsLight :: Animation
basicFunctionsLight = basicFunctions' LightTheme

basicFunctions' :: AnimationTheme -> Animation
basicFunctions' th = env th $ scene $ do
    UnaryFn {outline, fnType, ..} <- basicAToBObjs th

    -- basic a to b
    oShow outline
    fnType `oShowWith` oDraw
    forM_ [0..1] $ \_ ->
      basicAToBLoop (UnaryFn {..})

    -- add example
    BinaryFn {outline = newOutline, fnType = newFnType, ..} <- addExampleObjs th
    fork $ oTransform outline newOutline 1
    oTransform fnType newFnType 1

    forM_ [0..1] $ \_ ->
      addExampleLoop (BinaryFn {..})

--------------------------------------------------------------------------------

basicAToB :: Animation
basicAToB = basicAToB' DarkTheme

basicAToBLight :: Animation
basicAToBLight = basicAToB' LightTheme

basicAToB' :: AnimationTheme -> Animation
basicAToB' th = env th $ scene $ do
    UnaryFn {..} <- basicAToBObjs th
    oShow outline
    fnType `oShowWith` oDraw
    forM_ [0..3] $ \_ ->
      basicAToBLoop (UnaryFn {..})

basicAToBObjs :: AnimationTheme -> Scene s (UnaryFn s)
basicAToBObjs th = do
    input   <- oNew $ withThemeSecondary th $ translate (-0.1) 0$ mkCircle 0.5
    output  <- oNew $ withThemeSecondary th $ translate 1.9 0 $ mkRect 1 1
    outline <- oNew $ withThemePrimary th $ mkRoundedRect 5.5 1.4 0.7
    fnType  <- oNew $ center $ latexAlign "f :: a \\rightarrow b"
    return (UnaryFn {..})

basicAToBLoop :: UnaryFn s -> Scene s ()
basicAToBLoop UnaryFn {..} = do
    input `oShowWith` (setDuration 2 . oSlideInFromTop)
    oShow output
    output `oHideWith` (setDuration 2 . oSlideOutToBottom)


--------------------------------------------------------------------------------

addExample :: Animation
addExample = addExample' DarkTheme

addExampleLight :: Animation
addExampleLight = addExample' LightTheme

addExample' :: AnimationTheme -> Animation
addExample' th = env th $ scene $ do
    BinaryFn {..} <- addExampleObjs th
    oShow outline
    fnType `oShowWith` (setDuration 2 . oDraw)
    forM_ [0..3] $ \_ ->
      addExampleLoop (BinaryFn {..})

addExampleObjs :: AnimationTheme -> Scene s (BinaryFn s)
addExampleObjs th = do
    input1  <- oNew $ translate (-1.1) 0 $ mkCircleWithInt 0.5 9 th
    input2  <- oNew $ translate 2.005 0  $ mkCircleWithInt 0.5 10 th
    output  <- oNew $ translate 5.25 0   $ mkCircleWithInt 0.5 19 th
    outline <- oNew $ withThemePrimary th $ mkRoundedRect 13 1.6 0.8
    fnType  <- oNew $ withThemeSecondary th addFunction
    return (BinaryFn {..})

addExampleLoop :: BinaryFn s -> Scene s ()
addExampleLoop BinaryFn {..} = do
    fork $ input1 `oShowWith` (setDuration 2 . oSlideInFromTop)
    input2 `oShowWith` (setDuration 2 . oSlideInFromTop)
    oShow output
    output `oHideWith` (setDuration 2 . oSlideOutToBottom)

addFunction :: SVG
addFunction = center $ latexMono "(+) :: Int → Int → Int"


--------------------------------------------------------------------------------

type VarObject s a = (Sprite s, Var s a)
type ImpureFn s = (UnaryFn s, Object s SVG, Object s SVG, VarObject s Int)

impureFunction :: Animation
impureFunction = impureFunction' DarkTheme

impureFunction' :: AnimationTheme -> Animation
impureFunction' th = env th $ scene $ do
    fn @ (UnaryFn {..}, effectLabel, _, _) <- impureFunctionObjs th
    oShow outline
    oShow effectLabel
    fnType `oShowWith`(setDuration 2.5 . oDraw)
    forM_ [0..6] $ \_ ->
      impureFunctionLoop fn

impureFunctionLoop :: ImpureFn s -> Scene s ()
impureFunctionLoop (UnaryFn {..}, _, line, (cntSVG, cntVar)) = do
    input `oShowWith` (setDuration 2 . oSlideInFromTop)

    line `oShowWith` (setDuration 0.1 . oDraw)
    modifyVar cntVar (+ 1)
    line `oHideWith` (setDuration 0.1 . oDraw)

    oShow output
    output `oHideWith` (setDuration 2 . oSlideOutToBottom)

impureFunctionObjs :: AnimationTheme  -> Scene s (ImpureFn s)
impureFunctionObjs th = do
    -- Objects for unary function
    let outlineHeight = 1.4
    let outlineLength = 13
    input   <- oNew $ translate 5.55 (-1) $ mkCircleWithInt 0.5 6 th
    output  <- oNew $ translate (-5.45) (-1) $ mkCircleWithInt 0.5 (-6) th
    outline <- oNew $ translate 0 (-1) $ withThemePrimary th
                    $ mkRoundedRect outlineLength outlineHeight (outlineHeight / 2)
    fnType  <- oNew $ translate 0 (-1) $ withThemeSecondary th $ center
                    $ latexMono "int countedNegate(int x)"
    -- Variable, outline and label for counter
    cntVar  <- newVar 0
    cntSVG  <- newSprite (translate 1.95 2.25 . withThemePrimary th
                    . (\v -> mkCircleWithInt 0.5 v th) <$> unVar cntVar)
    label   <- oNew $ translate (-2.75) 2 $ latexMono "counter:"
    -- Line
    line    <- oNew $ smoothCurve (V2 0 ((outlineHeight / 2) - 1)) (V2 1.95 1.75)
    oModify line $ oZIndex .~ 0
    return (UnaryFn {..}, label, line, (cntSVG, cntVar))

smoothCurve :: RPoint -> RPoint -> SVG
smoothCurve from to
  = withFillOpacity 0
  $ mkPath [ MoveTo OriginAbsolute [from]
           , SmoothCurveTo OriginAbsolute [(from, to)]
           ]
