{-|
Module      : Main
Description : It contain one example
Copyright   : (c) Alexey Seledkov, 2022
License     : GPL-3
Maintainer  : qyutou@gmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Css

sampleStyle :: Css ()
sampleStyle = do
    "body" do
        background    "#000000"

    "header > nav" do
        background    "white"
        color         "#041"
        fontSize      "24px"
        padding       "20 0 20 0"
        position      "absolute"
        textTransform "uppercase"
        left          "0"
        right         "0"
        bottom        "-72px"

main :: IO ()
main = print sampleStyle
