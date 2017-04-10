{- RankProduct
Gregory W. Schwartz

Collects the functions pertaining to finding the rank product of a data set as
well as the associated p-value.
-}

{-# LANGUAGE BangPatterns #-}

module Statistics.RankProduct
    ( rankList
    , rankProduct
    , prerankProduct
    , rankProductPermutation
    ) where

-- Standard
import Data.Function (on)
import Data.List
import Data.Random

-- Cabal

-- Local
import Statistics.Types

-- | Rank transform a list.
rankList :: (Ord a) => [a] -> [Int]
rankList = fmap fst
         . sortBy (compare `on` (fst . snd))
         . zip [1..]
         . sortBy (compare `on` snd)
         . zip [1..]

-- | Get the rank product of a list of Entity.
rankProduct :: [Entity] -> [RankProductEntity]
rankProduct xs =
    fmap ( RankProductEntity
         . (** (1 / (genericLength . unEntity . head $ xs)))
         . fromIntegral
         . product
         )
        . transpose
        . fmap rankList
        . transpose
        . fmap unEntity
        $ xs

-- | Get the rank product of a pre-ranked data set [[ranked values for a gene in
-- different data sets]].
prerankProduct :: [RankEntity] -> [RankProductEntity]
prerankProduct xs =
    fmap
        ( RankProductEntity
        . (** (1 / (genericLength . unRankEntity . head $ xs)))
        . fromIntegral
        . product
        . unRankEntity
        )
        xs

-- | Get a random permutation of the RankEntity's.
permuteEntities :: [RankEntity] -> IO [RankEntity]
permuteEntities = fmap (fmap RankEntity . transpose)
                . mapM (sample . shuffle)
                . transpose
                . fmap unRankEntity

-- | Convert Bool to a number.
boolToNum :: (Num a) => Bool -> a
boolToNum False = 0
boolToNum True  = 1

-- | Get the rank product of a list of Entity as well as the permutation
-- p-value.
rankProductPermutation :: Permutations
                       -> [Entity]
                       -> IO [(RankProductEntity, PValue)]
rankProductPermutation (Permutations permutations) entities = do
    let ranked  = fmap RankEntity
                . transpose
                . fmap rankList
                . transpose
                . fmap unEntity
                $ entities
        obs     = prerankProduct ranked
        expTest (RankProductEntity o) = (<= o) . unRankProductEntity

    -- Lotsa space version.
    -- vals <- mapM (const (shuffleCosine xs ys)) . V.replicate nPerm $ 0
    -- let exps = V.filter (\x -> abs x >= abs obs) vals

    let successes :: [Int] -> Int -> IO [Int]
        successes !acc 0  = return acc
        successes !acc !n = do
            shuffledEntities <- permuteEntities ranked

            let res = prerankProduct shuffledEntities
                success = zipWith (\o -> boolToNum . expTest o) obs res

            successes (zipWith (+) acc success) (n - 1)

    exp <- successes (take (genericLength entities) [0,0..]) permutations

    let pVals =
            fmap (\e -> PValue $ (fromIntegral e) / (fromIntegral permutations)) exp

    return . zip obs $ pVals
