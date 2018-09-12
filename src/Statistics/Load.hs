{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the data frame from rows of a file.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Statistics.Load
    ( loadNamedEntities
    ) where

-- Standard
import Data.List (sort)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

-- Cabal

-- Local
import Statistics.Types

-- | Load entities from a list of rows.
loadNamedEntities :: V.Vector (Map.Map T.Text T.Text) -> [NamedEntity]
loadNamedEntities = fmap (uncurry NamedEntity)
                  . Map.toAscList
                  . Map.map (fmap snd . sort . F.toList)
                  . Map.fromListWith (Seq.><)
                  . fmap lookupInfo
                  . V.toList
  where
    lookupErr x = Map.findWithDefault (error $ "Missing column: " <> show x) x
    lookupInfo m =
      ( Name $ lookupErr "name" m
      , Seq.singleton ( lookupErr "replicate" m
                      , either error fst . T.double $ lookupErr "value" m
                      )
      )
