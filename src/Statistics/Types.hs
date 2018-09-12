{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}

module Statistics.Types where

-- Standard
import qualified Data.Text as T

-- Cabal

-- Local


-- Basic
newtype Delimiter         = Delimiter { unDelimiter :: Char } deriving (Read, Show)
newtype Name              = Name { unName :: T.Text } deriving (Eq, Ord, Show)
newtype Permutations      = Permutations Int deriving (Show)
newtype DefaultValue      = DefaultValue Double
newtype Entity            = Entity { unEntity :: [Double] } deriving (Show)
newtype RankEntity        = RankEntity { unRankEntity :: [Double] } deriving (Show)
newtype RankProductEntity = RankProductEntity
    { unRankProductEntity :: Double
    } deriving (Show)
newtype PValue            = PValue { unPValue :: Double } deriving (Show)

-- Advanced
data Sort = Ascending | Descending deriving (Eq, Ord, Read, Show)

data NamedEntity = NamedEntity { name :: Name
                               , values :: [Double]
                               }
                   deriving (Show)
