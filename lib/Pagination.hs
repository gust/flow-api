{-# LANGUAGE DeriveGeneric #-}
module Pagination where
import Data.Aeson(ToJSON, FromJSON)
import GHC.Generics

data Pagination = Pagination {
    perPage :: Int,
    page    :: Int,
    totalResults :: Int
} deriving (Show, Generic)

instance FromJSON Pagination
instance ToJSON Pagination

pOffset :: Pagination -> Int
pOffset pagination = (page pagination - 1) * (perPage pagination)
