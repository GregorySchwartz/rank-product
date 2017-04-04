{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

module Statistics.Types where

-- Standard

-- Cabal

-- Local


-- Basic
newtype Permutations      = Permutations Int
newtype Entity            = Entity { unEntity :: [Double] } deriving (Show)
newtype RankEntity        = RankEntity { unRankEntity :: [Int] }
newtype RankProductEntity = RankProductEntity
    { unRankProductEntity :: Double
    } deriving ((Show))
newtype PValue            = PValue { unPValue :: Double } deriving (Show)

-- Advanced
