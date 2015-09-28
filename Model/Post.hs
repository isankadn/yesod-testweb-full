{-# LANGUAGE ScopedTypeVariables #-}

module Model.Post where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

getPosts :: Int -> Int -> DB [Entity Post]
getPosts page postsPerPage
  | page > 0 && postsPerPage > 0 = selectList
    []
    [ Desc PostCreated
    , LimitTo postsPerPage
    , OffsetBy $ (page - 1) * postsPerPage
    ]
  | otherwise = return []

mkPostFromEvent :: Event -> Post
mkPostFromEvent Event {..} =
  Post eventUser eventCreated Nothing eventTitle eventContent