module Model.Post where

import Import

getPosts :: Int -> Int -> DB [Entity Post]
getPosts page postsPerPage = selectList
  []
  [ Desc PostCreated
  , LimitTo postsPerPage
  , OffsetBy $ (page - 1) * postsPerPage
  ]

mkPostFromEvent :: Event -> Post
mkPostFromEvent Event {..} =
  Post eventUser eventCreated Nothing eventTitle eventContent