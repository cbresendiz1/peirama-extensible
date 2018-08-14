{-#OPTIONS_GHC -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}

module State where

import Control.Eff
import Control.Eff.Writer.Lazy
import Control.Eff.Reader.Lazy

data State s v where
  Get :: State s s
  Put :: s -> State s ()
  Delay :: Eff '[State s] a -> State s a

-- {-# NOINLINE get #-}
-- get :: Member (State s) r => Eff r s
-- get = send Get
-- {-# RULES
--    "get/bind" forall k. get >>= k = send Get >>= k
-- #-}

{-# NOINLINE put #-}
put :: Member (State s) r => s -> Eff r ()
put s = send  (Put s)
{-# RULES
  "put/bind"   forall k v. put v >>= k = send (Put v) >>= k
#-}
{-# RULES
  "put/semibind" forall k v. put v >> k = send (Put v) >>= (\() -> k)
#-}

onDemand :: (Member (State s) r) => Eff '[State s] v -> Eff r v
onDemand = send . Delay

--runState' :: Eff (State s ': r) w -> s -> Eff r (w,s)
--runState' m s =
--  handle_relay_s s
--  (\s0 x -> return (x, s0))
--  (\s0 sreq k -> case sreq of
--      Get -> k s0 s0
--      Put s1 -> k s1 ()
--      Delay m1 -> let ~(x, s1) = run $ runState' m1 s0
--                  in k s1 x)
--  m

runState :: Eff (State s ': r) w
         -> s
         -> Eff r (w,s)
runState (Val x) s = return (x, s)
runState (E u0 q) s0 = case decomp u0 of
  Right Get -> runState (q ^$ s0) s0
  Right (Put s1) -> runState (q ^$ ()) s1
  Right (Delay m1) -> let ~(x, s1) = run $ runState m1 s0
                      in runState (q ^$ x) s1
  Left u -> E u (singleK (\x -> runState (q ^$ x) s0))

--modify :: (Member (State s) r) => (s -> s) -> Eff r ()
--modify f = get >>= put . f

evalState :: Eff (State s ': r) w -> s -> Eff r w
evalState m s = fmap fst . flip runState s $ m

execState :: Eff (State s ': r) w -> s -> Eff r s
execState m s = fmap snd . flip runState s $ m

runStateR :: Eff (Writer s ': Reader s ': r) w -> s -> Eff r (w, s)
runStateR m0 s0 = loop s0 m0
  where
    loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w, s)
    loop s (Val x) = return (x, s)
    loop s (E u0 q) = case decomp u0 of
      Right (Tell w) -> k w ()
      Left u -> case decomp u of
        Right Reader -> k s s
        Left u1 -> E u1 (singleK (k s))
      where k x = qComp q (loop x)

--runStateBack0 :: Eff '[State s] a -> (a, s)
--runStateBack0 m =
--  let (x, s) = go s m in
--    (x, s)
--  where
--    go :: s -> Eff '[State s] a -> (a, s)
    
                       
