{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, forAll, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified Gen

import           Swarm.DB.Replica (open, get, newTextReplica)
import           Swarm.RON.Status (Status (Status), notOpen, ok)

main = $defaultMainGenerator

prop_uninitialized_replica = property $ do
    replica <- liftIO newTextReplica
    objectId <- forAll Gen.uuid
    liftIO (get objectId replica) >>= expectLeft >>= (=== Status notOpen "")

prop_put_get = property $ do
    objectId <- forAll Gen.uuid
    replica <- liftIO newTextReplica
    liftIO (open "/tmp/swarm.3" replica) >>= (=== Status ok "")
    liftIO (get objectId replica) >>= expectLeft >>= (=== Status notOpen "")

expectLeft :: (MonadTest m, HasCallStack) => Either a x -> m a
expectLeft = \case
    Left x  -> pure x
    Right _ -> withFrozenCallStack $ failWith Nothing "unexpected Right"
