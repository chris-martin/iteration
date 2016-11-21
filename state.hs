#!/usr/bin/env stack
-- stack --resolver lts-7.8 --install-ghc runghc --package classy-prelude

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad.Trans.State (StateT, execStateT, put, get)

-- | Print a running sum total, and return the product.
f :: forall a. (Num a, Show a) => [a] -> IO a
f xs =
  let
    initialState = (fromInteger 0, fromInteger 1)

    visit :: a -> StateT (a, a) IO ()
    visit x = do
        get >>= \state -> put $ bimap (+ x) (* x) state
        get >>= \state -> lift $ traverse_ putStr [tshow $ fst state, " "]
  in
    do
      state <- execStateT (traverse_ visit xs) initialState
      return $ snd state

main :: IO ()
main = do
    x <- f [1..4]
    traverse_ putStr ["| ", tshow x, "\n"]
