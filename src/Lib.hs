{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE TypeOperators, GADTs, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module Lib
    ( someFunc
    ) where

import Control.Eff
import Control.Eff.Exception

import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy

newtype TooBig = TooBig Int deriving (Eq, Show)

runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

sum2 :: Member (Reader Int) r
     => Member (Reader Float) r
     => Eff r Float
sum2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1 :: Int)) + (v2 + (2 :: Float))

writeAll :: (Member (Writer a) e)
         => [a]
         -> Eff e ()
writeAll = mapM_ tell

sumAll :: (Num a, Member (State a) e)
       => [a]
       -> Eff e ()
sumAll = mapM_ (modify . (+))

writeAndAdd :: (Member (Writer a) e, Member (State a) e, Num a)
            => [a]
            -> Eff e ()
writeAndAdd l = do
    writeAll l
    sumAll l

sumEff :: (Num a) => [a] -> a
sumEff l = let ((), s) = run $ runState (sumAll l) 0
           in s

lastEff :: [a] -> Maybe a
lastEff l = let ((), a) = run $ runLastWriter $ writeAll l
            in a

lastAndSum :: (Num a) => [a] -> (Maybe a, a)
lastAndSum l = let (((), total), lst) =
                     run $ runLastWriter $ runState (writeAndAdd l) 0
               in (lst, total)

data Move x where
  Move :: Move ()

handUp :: Eff (Move ': r) a -> Eff r a
handUp (Val x) = return x
handUp (E u q) = case decomp u of
  Right Move -> handDown $ qApp q ()
  Left u0    -> E u0 ident >>= handUp . qApp q

handDown :: Eff (Move ': r) a -> Eff r a
handDown (Val x) = return x
handDown (E u q) = case decomp u of
  Right Move -> handUp $ qApp q ()
  Left u0    -> E u0 ident >>= handDown . qApp q

someFunc :: IO ()
someFunc = putStrLn "someFunc"
