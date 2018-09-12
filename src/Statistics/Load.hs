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
import Data.List (sort, find)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

-- Cabal

-- Local
import Statistics.Types

-- | Load entities from a list of rows.
loadNamedEntities ::
  Maybe DefaultValue -> V.Vector (Map.Map T.Text T.Text) -> [NamedEntity]
loadNamedEntities defVal rows =
  fmap (uncurry NamedEntity)
    . Map.toAscList
    . Map.map (fmap snd . supplyReplicates defVal allReplicates . F.toList)
    . Map.fromListWith (Seq.><)
    . fmap lookupInfo
    . V.toList
    $ rows
  where
    allReplicates = Set.fromList . fmap (lookupErr "replicate") . V.toList $ rows
    lookupErr x = Map.findWithDefault (error $ "Missing column: " <> show x) x
    lookupInfo m =
      ( Name $ lookupErr "name" m
      , Seq.singleton ( lookupErr "replicate" m
                      , either error fst . T.double $ lookupErr "value" m
                      )
      )

-- | Give a default value to missing replicates.
supplyReplicates :: Maybe DefaultValue
                 -> Set.Set T.Text
                 -> [(T.Text, Double)]
                 -> [(T.Text, Double)]
supplyReplicates Nothing _ xs = sort xs
supplyReplicates (Just (DefaultValue defVal)) allReplicates xs =
    Map.toAscList . F.foldl' supply repMap . Set.toList $ allReplicates
  where
    supply m x = if Map.member x m then m else Map.insert x defVal m
    repMap = Map.fromList xs
