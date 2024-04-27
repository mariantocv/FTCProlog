{-# OPTIONS_GHC -fno-warn-tabs #-} 

module Main where

import System.IO
import Interprete

main :: IO()
main = 
    do
    	hSetEncoding stdin localeEncoding
    	hSetEncoding stdout localeEncoding
        hSetBuffering stdout NoBuffering
        interprete