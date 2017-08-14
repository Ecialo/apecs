{-# LANGUAGE StandaloneDeriving, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Control.ECS.Storage where

import Control.Monad
import Control.Monad.Trans

type ID = Int

class SStorage IO (Storage c) => Component c where
  type Storage c :: *

class Monad m => SStorage m s where
  type SElem s :: *
  type SSafeElem s :: *

  sEmpty    :: m s
  sSlice    :: s -> m [ID]
  sMember   :: s -> ID -> m Bool
  sDestroy  :: s -> ID -> m ()
  sRetrieve :: s -> ID -> m (SSafeElem s)
  sStore    :: s -> SSafeElem s -> ID -> m ()
  sOver     :: s -> (SElem s -> SElem s) -> m ()
  sForC     :: s -> (SElem s -> m a) -> m ()

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( Monad m, SStorage m sa, SStorage m sb) => SStorage m (sa, sb) where
  type SSafeElem (sa, sb) = (SSafeElem sa, SSafeElem sb)
  type SElem     (sa, sb) = (SSafeElem sa, SSafeElem sb)

  sEmpty = liftM2 (,) sEmpty sEmpty
  sSlice    (sa,sb) = sSlice sa >>= filterM (sMember sb)
  sMember   (sa,sb) ety = liftM2 (&&) (sMember sa ety) (sMember sb ety)
  sDestroy  (sa,sb) ety = sDestroy sa ety >> sDestroy sb ety
  sRetrieve (sa,sb) ety = liftM2 (,) (sRetrieve sa ety) (sRetrieve sb ety)
  sStore    (sa,sb) (xa,xb) ety = sStore sa xa ety >> sStore sb xb ety

  sOver s f = do sl <- sSlice s
                 forM_ sl $ \ety ->
                   do r  <- sRetrieve s ety
                      sStore s (f r) ety

  sForC s f = do sl <- sSlice s
                 forM_ sl $ \ety ->
                   do r <- sRetrieve s ety
                      f r