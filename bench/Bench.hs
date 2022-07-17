{-|
Module      : Main
Description : Benchmarks
Copyright   : (c) Alexey Seledkov, 2022
License     : GPL-3
Maintainer  : qyutou@gmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Criterion
import Criterion.Main (defaultMain)
import qualified Data.Text as T
import Css

testAppend :: Int -> Css () -> Css ()
testAppend 0 x = x
testAppend n x = testAppend (n - 1) x <> ("selector" ? ("property" |> (T.pack . show) n))

testAppend' :: Int -> Css ()
testAppend' n = testAppend n ("selector" ? ("property" |> "value"))

main :: IO ()
main = do
    let [c1, c2, c3, c4, c5, c6] = [ testAppend' 1, testAppend' 10, testAppend' 100, testAppend' 1000, testAppend' 10000, testAppend' 100000]
    defaultMain 
        [ bgroup "Rendering"
            [ bench "Render 1 element test"       (whnf renderCss c1)
            , bench "Render 10 elements test"     (whnf renderCss c2)
            , bench "Render 100 elements test"    (whnf renderCss c3)
            , bench "Render 1000 elements test"   (whnf renderCss c4)
            , bench "Render 10000 elements test"  (whnf renderCss c5)
            , bench "Render 100000 elements test" (whnf renderCss c6)
            ]
        ]
