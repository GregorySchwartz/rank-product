{- rank-product
Gregory W. Schwartz

Get the rank product of a data frame.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Options.Generic
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV

-- Local
import Statistics.Types
import Statistics.Load
import Statistics.RankProduct

-- | Command line arguments
data Options = Options { permutations :: Maybe Int
                                     <?> "([1000] | INT) Number of permutations for rank product p-value."
                       , delimiter    :: Maybe Char
                                     <?> "([,] | CHAR) The delimiter of the CSV file. Format is name,replicate,value."
                       , sortType         :: Maybe Sort
                                     <?> "([Ascending] | Descending) Ascending ranks 1 as the lowest value while Descending ranks the highest value as 1."
                       }
               deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short x         = firstLetter x

instance ParseField Sort

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

main :: IO ()
main = do
    opts <- getRecord "rank-product, Gregory W. Schwartz.\
                      \ Compute the rank product for a data frame.\
                      \ Format is name,replicate,value."

    let permutations' =
          Permutations . fromMaybe 1000 . unHelpful . permutations $ opts
        delim'        =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        sortType'     = fromMaybe Ascending . unHelpful . sortType $ opts
        decodeOpt       = CSV.defaultDecodeOptions
                            { CSV.decDelimiter =
                                fromIntegral (ord . unDelimiter $ delim')
                            }
        encodeOpt       = CSV.defaultEncodeOptions
                            { CSV.encDelimiter =
                                fromIntegral (ord . unDelimiter $ delim')
                            }

    entities <- loadNamedEntities
              . either error snd
              . CSV.decodeByNameWith decodeOpt
            <$> B.getContents

    rankedEntities <-
      namedRankProductPermutation permutations' sortType' entities

    let header = "name,rank,pvalue\n"
        body = L.over L._3 unPValue
             . L.over L._2 unRankProductEntity
             . L.over L._1 unName
           <$> rankedEntities

    B.putStrLn . (header <>) $ CSV.encodeWith encodeOpt body
