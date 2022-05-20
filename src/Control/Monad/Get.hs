{-# LANGUAGE FunctionalDependencies
           , DefaultSignatures
           , FlexibleInstances
           , TypeFamilies -- equality constraints for default signatures
           , UndecidableInstances -- lifted instances
   #-}


module Control.Monad.Get (
MonadGet (..), get,
MonadState (..), modify, modify', getsDefault, stateDefault, putDefault,
) where


import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.RWS.CPS as RWS
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.CPS as Strict


class (Monad m)=> MonadGet s m | m -> s where
{-^
View the state of a 'Monad'.

Law: @'get' *> mx@ = @mx@

A default definition of 'gets' is provided for instances of 'MonadTrans' wrapping instances of @MonadGet@.
-}
    gets :: (s -> a) -> m a
    {-^ Apply a function to the current state and return the result. -}
    default gets :: (m ~ t n, MonadTrans t, MonadGet s n)=> (s -> a) -> m a
    gets = lift . gets
    -- gets = liftF' gets
    {-# INLINE gets #-}

get :: (MonadGet s m)=> m s
{-^ Return the current state. -}
get = gets id
{-# INLINE get #-}


class (MonadGet s m)=> MonadState s m | m -> s where
{-^
Update the state of a 'Monad'.

Laws:
@'get' '>>=' 'put'@ = @'pure' ()@
@'put' s '*>' 'put' s'@ = @'put' s'@

Default definitions of 'state' are provided for instances of 'MonadTrans' wrapping instances of @MonadState@.
-}
    state :: (s -> (a, s)) -> m a
    {-^ Use a function to modify the state and return a result.

    If you manually define @state@, make sure that @state f@ = @do{ ~(x, s) <- 'gets' f; 'put' s; 'pure' x }@
    -}
    default state ::
        (m ~ t n, MonadTrans t, MonadState s n)=> (s -> (a, s)) -> m a
    state = lift . state
    -- state = liftF' state
    {-# INLINE state #-}

    put :: (MonadState s m)=> s -> m ()
    {-^ Replace the state.

    If you manually define @put@, make sure that @put s@ = @'state' (\ _ -> ((), s))@
    -}
    default put ::
        (m ~ t n, MonadTrans t, MonadState s n)=> s -> m ()
    put = lift . put
    -- put = liftF' put
    {-# INLINE put #-}

modify, modify' :: (MonadState s m)=> (s -> s) -> m ()
{-^ Use a function to update the state. @modify'@ evaluates the new state before returning; @modify@ may not.
-}
modify f = state ((,) () . f)
{-# INLINE modify #-}
modify' f = gets f >>= (put $!)
{-# INLINE modify' #-}

stateDefault :: (MonadState s m)=> (s -> (a, s)) -> m a
{-^ @stateDefault@ is a suitable definition for 'state' if you manually define 'put' and 'gets'. -}
stateDefault f = gets f >>= \ ~(x, s) -> x <$ put s
{-# INLINE stateDefault #-}

putDefault :: (MonadState s m)=> s -> m ()
{-^ @putDefault@ is a suitable definition for 'put' if you manually define 'state'. -}
putDefault s = state (\ _ -> ((), s))
{-# INLINE putDefault #-}

getsDefault :: (MonadState s m)=> (s -> a) -> m a
{-^ @getsDefault is a suitable definition for 'gets' if you manually define 'state'. -}
getsDefault f = state (\ s -> (f s, s))
{-# INLINE getsDefault #-}


instance (Monad m)=> MonadGet s (Lazy.StateT s m) where
    gets = getsDefault
    {-# INLINE gets #-}

instance (Monad m)=> MonadState s (Lazy.StateT s m) where
    state = Lazy.state
    {-# INLINE state #-}
    put = putDefault
    {-# INLINE put #-}

instance (Monad m)=> MonadGet s (Strict.StateT s m) where
    gets = getsDefault
    {-# INLINE gets #-}

instance (Monad m)=> MonadState s (Strict.StateT s m) where
    state = Strict.state
    {-# INLINE state #-}
    put = putDefault
    {-# INLINE put #-}

instance (Monad m)=> MonadGet s (RWS.RWST r w s m) where
    gets = RWS.gets
    {-# INLINE gets #-}

instance (Monad m)=> MonadState s (RWS.RWST r w s m) where
    state = RWS.state
    {-# INLINE state #-}
    put = RWS.put
    {-# INLINE put #-}


--- Lifted Instances ---
instance (MonadGet s m, Monoid w)=> MonadGet s (AccumT w m)
instance (MonadState s m, Monoid w)=> MonadState s (AccumT w m)

instance (MonadGet s m)=> MonadGet s (ContT r m)
instance (MonadState s m)=> MonadState s (ContT r m)

instance (MonadGet s m)=> MonadGet s (ExceptT e m)
instance (MonadState s m)=> MonadState s (ExceptT e m)

instance (MonadGet s m)=> MonadGet s (MaybeT m)
instance (MonadState s m)=> MonadState s (MaybeT m)

instance (MonadGet s m)=> MonadGet s (ReaderT r m)
instance (MonadState s m)=> MonadState s (ReaderT r m)

instance (MonadGet s m)=> MonadGet s (SelectT r m)
instance (MonadState s m)=> MonadState s (SelectT r m)

instance (MonadGet s m, Monoid w)=> MonadGet s (Lazy.WriterT w m)
instance (MonadState s m, Monoid w)=> MonadState s (Lazy.WriterT w m)

instance (MonadGet s m, Monoid w)=> MonadGet s (Strict.WriterT w m)
instance (MonadState s m, Monoid w)=> MonadState s (Strict.WriterT w m)


{- Does it make more sense to provide default MonadTrans lifted methods, or to interdefine the methods and provide a newtype WrapMonadTrans for deriving via WrapMonadTrans?
-}

-- newtype WrapMonadTrans (t :: (Type -> Type) -> Type -> Type) m a = WrapMonadTrans (t m a)
--   deriving (Functor, Applicative, Monad)

-- instance (MonadTrans t)=> MonadTrans (WrapMonadTrans t) where
--     lift = WrapMonadTrans . lift
--     {-# INLINE lift #-}

-- instance
--   (MonadTrans t, Monad (t m), MonadGet s m)=> MonadGet s (WrapMonadTrans t m) where
--     gets = lift . gets
--     {-# INLINE gets #-}

-- instance
--   (MonadTrans t, Monad (t m), MonadState s m)=>
--   MonadState s (WrapMonadTrans t m) where
--     state = lift . state
--     {-# INLINE state #-}
--     put = lift . put
--     {-# INLINE put #-}

liftF' :: (MonadTrans t, Monad m)=> (a -> m b) -> a -> t m b
{-^ Lift a function into a monad transformer, strictly evaluating the function. This can help force dictionary unpacking. -}
liftF' = (!.!) lift
{-# INLINE liftF' #-}

(!.!) :: (b -> c) -> (a -> b) -> a -> c
(!.!) f g = f `seq` g `seq` \ x -> f (g x)
{-# INLINE (!.!) #-}
