{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Render.GlossInterface where

import Prelude hiding (id, (.))

import FRP.Yampa
import Settings

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

import Data.IORef
import System.Exit
import System.Process

import Control.Monad

import qualified Sound.Tidal.Context as T

import Network.Socket
import Network.Socket.ByteString as SB

import Vivid.OSC
import Control.Concurrent

exitSuperCollider :: IO()
exitSuperCollider = do
   (a:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "57122")
   s <- socket (addrFamily a) Datagram defaultProtocol
   connect s (addrAddress a)
   SB.send s $ encodeOSC $
      OSC "/haskellExit" []
   threadDelay 1000
   return ()

playYampa :: 
    Display ->
    Color ->
    Int ->
    SF (Event [G.Event], (Int,Int)) (Picture, T.Pattern T.ControlMap, Bool) ->
    Maybe ProcessHandle -> 
    IO ()

playYampa display color frequency network supercolliderProcess = 
  do
    --TODO this doesn't belong here
    tidal <- T.startTidal (T.superdirtTarget {T.oLatency = 0.6, T.oAddress = "127.0.0.1", T.oPort = 57120}) (T.defaultConfig {T.cFrameTimespan = 1/10})
    let p = T.streamReplace tidal

    vPic <- newIORef Blank
    events <- newIORef NoEvent

    handle <- reactInit 
        (return (NoEvent, Settings.windowSize))
        (\_ changed (pic, pat, exitFlag) -> do
            when exitFlag $ do
               case supercolliderProcess of
                 Just scPID -> do
                    putStrLn "Cleaning up SuperCollider" 
                    exitSuperCollider
                    --cleanupProcess (Nothing, Nothing, Nothing, scPID) 
                    --waitForProcess scPID >>= print
                    exitSuccess
                 Nothing -> exitSuccess 
            if changed then vPic `atomicWriteIORef` pic else return ()
            p 1 pat
            return False)
        network
    
    _ <- react handle (infts, Just (NoEvent, Settings.windowSize))

    -- Since `react` requires nonzero time intervals,
    -- we pass infinitesimal time intervals and accumulate them on
    -- the variable `t`. Then every frame `delta` is corrected by `t`.
    G.playIO
        display
        color
        frequency
        infts -- initial t. This is for initial step
        (const $ readIORef vPic)
        -- does not handle multiple events within one update cycle
        -- TODO make events :: IORef [Event]?
        -- this (should) accumulates events, and only processes them in the \delta fxn below
        (\e t -> modifyIORef events (addGameEvent e) >> return (t+infts))
        (\delta t ->
            let 
                delta' = realToFrac delta - t
              in
                if delta' > 0
                  then do
                    e <- readIORef events
                    _ <- react handle (delta', Just (e, Settings.windowSize) )
                    atomicWriteIORef events NoEvent
                    return 0.0
                  else
                    return (-delta'))
  where
    infts = 0.01 / fromIntegral frequency
    addGameEvent gameE yampaE  = case yampaE of
      NoEvent -> Event [gameE]
      Event es -> Event (gameE:es)
