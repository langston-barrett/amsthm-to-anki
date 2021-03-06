-- |
-- Module      :  Test.Util
-- Copyright   :  (c) Langston Barrett 2017
-- License     :  MPL-2.0
-- Maintainer  :  siddharthist
--
-- Testing utilities.
-- "forall a."
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

-- Constrable
-- Constrable
-- "import safe" is enforced.
module Test.Util where

import ClassyPrelude
import Data.Data
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HU
import Text.LaTeX (render)

-- My modules
import Extract
import Types

isLeft :: Either.Either a b -> Bool
isLeft = Either.either (\_ -> True) (\_ -> False)

assertLeft :: Either.Either a b -> HU.Assertion
assertLeft e = True @=? isLeft e

assertRight :: Either.Either a b -> HU.Assertion
assertRight e = False @=? isLeft e

assertJust
  :: forall a.
     Maybe a -> HU.Assertion
assertJust m = True @=? Maybe.isJust m

-- | Things that can be compared by constructor
-- The OVERLAP pragma is used instead of the OverlappingInstances language
-- extension.
class Constrable a where
  constr :: a -> Constr

-- | All Data instances are trivially comparable by constructor
instance Data a =>
         Constrable a where
  constr = toConstr

-- | Compare directly using constructors. Will explode on types that are eager
-- in their constructors.
instance Constrable a =>
         Constrable (b -> a) where
  constr constructor = constr (constructor undefined)

-- | Assert that a piece of data uses a certain constructor
(=|=)
  :: forall a b.
     (Constrable a, Constrable b)
  => a -> b -> HU.Assertion
x =|= y = constr x @=? constr y

-- Some functions to construct text test cases. Basically low-budget HaTeX. If
-- you don't trust these (you shouldn't), just break a test and see how they
-- work.
makeEnvText :: Text -> Text -> Text
makeEnvText env content =
  "\\begin{" ++ env ++ "}" ++ content ++ "\\end{" ++ env ++ "}"

makeNoteText :: Text -> Text -> Text
makeNoteText f b =
  makeEnvText "note" $ makeEnvText "field" f ++ makeEnvText "field" b

showNote :: Notecard -> Text
showNote = render . notecardToLaTeX
