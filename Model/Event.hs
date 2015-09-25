module Model.Event where

import Import

getEvents :: DB [Entity Event]
getEvents = selectList
  []
  [ Desc EventStartDate
  ]