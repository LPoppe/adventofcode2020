{-# LANGUAGE RankNTypes #-}
module Util where

import Protolude

readText :: forall a. Read a => Text -> Maybe a
readText = readMaybe . toS

unsafeReadText :: (HasCallStack, Read a) => Text -> a
unsafeReadText = fromMaybe (panic "wut") . readText

andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen = (=<<)