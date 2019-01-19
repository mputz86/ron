{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Prelude (
    module X,
    identity,
    lastDef,
    maximumDef,
) where

-- base
import           Control.Applicative as X (Alternative, Applicative, liftA2,
                                           many, optional, pure, some, (*>),
                                           (<*), (<*>), (<|>))
import           Control.Exception as X (evaluate)
import           Control.Monad as X (Monad, guard, unless, void, when, (<=<),
                                     (=<<), (>=>), (>>=))
import           Control.Monad.Fail as X (MonadFail, fail)
import           Control.Monad.IO.Class as X (MonadIO)
import           Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import           Data.Char as X (Char, chr, ord, toLower, toUpper)
import           Data.Coerce as X (coerce)
import           Data.Data as X (Data)
import           Data.Either as X (Either (Left, Right))
import           Data.Eq as X (Eq, (/=), (==))
import           Data.Foldable as X (Foldable, fold, foldMap, foldl', foldr,
                                     length, minimumBy, null, toList)
import           Data.Function as X (const, flip, on, ($), (.))
import           Data.Functor as X (Functor, fmap, ($>), (<$), (<$>))
import           Data.Functor.Identity as X (Identity)
import           Data.Int as X (Int, Int16, Int32, Int64, Int8)
import           Data.IORef as X (IORef, atomicModifyIORef', newIORef)
import           Data.List as X (filter, intercalate, map, partition, repeat,
                                 replicate, span, splitAt, take, takeWhile,
                                 unlines, unwords, zip, (++))
import           Data.List.NonEmpty as X (NonEmpty, nonEmpty)
import           Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe,
                                  maybeToList)
import           Data.Monoid as X (Monoid, mempty)
import           Data.Ord as X (Ord, Ordering (EQ, GT, LT), compare, comparing,
                                max, min, (<), (<=), (>), (>=))
import           Data.Ratio as X ((%))
import           Data.Semigroup as X (Semigroup, sconcat, (<>))
import           Data.String as X (String)
import           Data.Traversable as X (for, sequence, sequenceA, traverse)
import           Data.Tuple as X (fst, snd, uncurry)
import           Data.Word as X (Word, Word16, Word32, Word64, Word8)
import           GHC.Enum as X (Bounded, Enum, fromEnum, maxBound, minBound,
                                pred, succ, toEnum)
import           GHC.Err as X (error, undefined)
import           GHC.Exts as X (Double)
import           GHC.Generics as X (Generic)
import           GHC.Integer as X (Integer)
import           GHC.Num as X (Num, negate, subtract, (*), (+), (-))
import           GHC.Real as X (Integral, fromIntegral, mod, realToFrac, round)
import           GHC.Stack as X (HasCallStack)
import           System.IO as X (FilePath, IO)
import           Text.Show as X (Show, show)

#ifdef VERSION_bytestring
import           Data.ByteString as X (ByteString)
#endif

#ifdef VERSION_containers
import           Data.Map.Strict as X (Map)
#endif

#ifdef VERSION_deepseq
import           Control.DeepSeq as X (NFData, force)
#endif

#ifdef VERSION_hashable
import           Data.Hashable as X (Hashable, hashUsing, hashWithSalt)
#endif

#ifdef VERSION_mtl
import           Control.Monad.Except as X (ExceptT)
import           Control.Monad.Reader as X (ReaderT (ReaderT), ask, reader,
                                            runReaderT)
import           Control.Monad.State.Strict as X (State, StateT, evalState,
                                                  evalStateT, execStateT,
                                                  modify', runState, state)
import           Control.Monad.Trans as X (MonadTrans, lift)
import           Control.Monad.Writer.Strict as X (tell)
#endif

#ifdef VERSION_text
import           Data.Text as X (Text)
#endif

#ifdef VERSION_time
import           Data.Time as X (UTCTime)
#endif

#ifdef VERSION_unordered_containers
import           Data.HashMap.Strict as X (HashMap)
#endif

--------------------------------------------------------------------------------

import           Data.List (last, maximum)

identity :: a -> a
identity x = x

lastDef :: a -> [a] -> a
lastDef def = list' def last

list' :: b -> ([a] -> b) -> [a] -> b
list' onEmpty onNonEmpty = \case
    [] -> onEmpty
    xs -> onNonEmpty xs

maximumDef :: Ord a => a -> [a] -> a
maximumDef def = list' def maximum