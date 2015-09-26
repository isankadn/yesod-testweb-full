module Model.Post where

import Import

getPosts :: Int -> Int -> DB [Entity Post]
getPosts page postsPerPage = selectList
  []
  [ Desc PostCreated
  , LimitTo postsPerPage
  , OffsetBy $ (page - 1) * postsPerPage
  ]