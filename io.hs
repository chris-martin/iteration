#!/usr/bin/env stack
-- stack --resolver lts-7.8 --install-ghc runghc --package classy-prelude

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude

-- | Print a running sum total, and return the product.
f :: forall a. (Num a, Show a) => [a] -> IO a
f xs =
  let
    initialState = (fromInteger 0, fromInteger 1)

    visit :: (IORef (a, a)) -> a -> IO ()
    visit stateRef x = do
        readIORef stateRef >>= \state -> writeIORef stateRef $ bimap (+ x) (* x) state
        readIORef stateRef >>= \state -> traverse_ putStr [tshow $ fst state, " "]
  in
    do
      stateRef <- newIORef initialState
      traverse_ (visit stateRef) xs
      state <- readIORef stateRef
      return $ snd state

main :: IO ()
main = do
    x <- f [1..4]
    traverse_ putStr ["| ", tshow x, "\n"]
