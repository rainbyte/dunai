-- Part of this code is taken and adapted from:
-- https://wiki.haskell.org/WxHaskell/Quick_start#Hello_world_in_wxHaskell
module Main where

import Control.Monad
import Data.IORef
import Data.MonadicStreamFunction
import Graphics.UI.WX

main :: IO ()
main = start hello

hello :: IO ()
hello
  = do f      <- frame      []
       lenLbl <- staticText f [ text := "0" ]
       entry  <- textEntry  f []
       quit   <- button     f [text := "Quit", on command := close f]

       hndlr  <- pushReactimate_ (myMSF lenLbl entry)
       set entry [ on update := hndlr ]
       
       set f [layout := margin 10 (column 5 [ floatCentre (widget lenLbl)
                                            , floatCentre (widget entry)
                                            , floatCentre (widget quit)
                                            ] )]

myMSF lbl entry = liftMStreamF_ (get entry text)
              >>> arr (show.length)
              >>> liftMStreamF (\t -> set lbl [ text := t ])

pushReactimate :: MStreamF IO a b -> IO (a -> IO b)
pushReactimate msf = do
  msfRef <- newIORef msf
  return $ \a -> do
              msf' <- readIORef msfRef
              (b, msf'') <- unMStreamF msf' a
              writeIORef msfRef msf''
              return b

pushReactimate_ :: MStreamF IO () () -> IO (IO ())
pushReactimate_ msf = do
  f <- pushReactimate msf
  return (void (f ()))
