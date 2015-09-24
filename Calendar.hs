module Calendar where

import Import hiding (for)

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.MonthDay

import Helpers

data Calendar = Calendar
  { months :: [Month]
  }
  deriving (Show)

data Month = Month
  { name :: Text
  , year :: Integer
  , current :: Bool
  , weeks :: [[Int]]
  }
  deriving (Show)

calendarWidget calendar = [whamlet|
  $forall m <- months calendar
    <table #calendar :(not $ current m):hidden>
      <thead>
        <tr>
          <th .arrow><
          <th colspan=5>#{name m} - #{show $ year m}
          <th .arrow>>
      <tbody>
        <tr>
          <th>_{MsgMondayShort}
          <th>_{MsgTuesdayShort}
          <th>_{MsgWednesdayShort}
          <th>_{MsgThursdayShort}
          <th>_{MsgFridayShort}
          <th>_{MsgSaturdayShort}
          <th>_{MsgSundayShort}
        $forall week <- weeks m
          <tr>
            $forall day <- week
              <td>#{show day}
|]

intToMonth :: Int -> Text
intToMonth 1 = "tammikuu"
intToMonth 2 = "helmikuu"
intToMonth 3 = "maaliskuu"
intToMonth 4 = "huhtikuu"
intToMonth 5 = "toukokuu"
intToMonth 6 = "kesäkuu"
intToMonth 7 = "heinäkuu"
intToMonth 8 = "elokuu"
intToMonth 9 = "syyskuu"
intToMonth 10 = "lokakuu"
intToMonth 11 = "marraskuu"
intToMonth 12 = "joulukuu"
intToMonth _ = ""

-- 12 months: current month, 5 months in the past and 6 months to the future
calendar :: Day -> Calendar
calendar day =
  Calendar $ for (months 2015 9) $ \(y, m) ->
    Month (intToMonth m) y (9 == m) $ oneMonth y m
  where
    months :: Integer -> Int -> [(Integer, Int)]
    months year month = for [0..11] $ \i -> validYearAndMonth year (month - 5 + i)
    validYearAndMonth :: Integer -> Int -> (Integer, Int)
    validYearAndMonth y m =
      if m < 1
        then (y - 1, 12 + m) -- previous year
        else if m > 12
          then (y + 1, m - 12) -- next year
          else (y,m) -- all is good

prevMonthAndYear :: Integer -> Int -> (Integer, Int)
prevMonthAndYear year 1 = (year - 1, 12)
prevMonthAndYear year month = (year, month - 1)

oneMonth :: Integer -> Int -> [[Int]]
oneMonth year month = toWeeks $ allDaysInMonth year month

-- break 42 days in to 6 weeks
toWeeks :: [Int] -> [[Int]]
toWeeks (d1:d2:d3:d4:d5:d6:d7:days) = [d1,d2,d3,d4,d5,d6,d7]:toWeeks days
toWeeks _ = []

-- return a list of 42 ints that has all the days in given month
-- and some additional days from previous and next month
allDaysInMonth :: Integer -> Int -> [Int]
allDaysInMonth year month =
  let
    -- first day of this month (week date)
    firstDay = dayOfWeek $ fromGregorian year month 1
    (prevYear, prevMonth) = prevMonthAndYear year month
    -- days in previous month
    daysPrevMonth = daysInMonth prevYear prevMonth
    -- days in previous month that should be shown with this month
    -- meaning days from monday to the day when this months starts
    -- if this month starts from monday then this is empty
    prevMonthDays = reverse [daysPrevMonth - i + 1 | i <- [1..firstDay]]
    -- days in current month
    currentMonth = daysInMonth year month
    -- last day of the month (week day)
    lastDay = dayOfWeek $ fromGregorian year month currentMonth
    -- days in next month that should be shown with this month
    -- meaning days to sunday from the day when this month ends
    -- if this month ends in sunday then this is empty
    nextMonthDays = [1..6-lastDay]
    -- list of days in this month
    currentMonthDays = [1..currentMonth]
    -- all the days together
    allDays = prevMonthDays ++ currentMonthDays ++ nextMonthDays
    -- rare case where in a leap year where February starts in monday and
    -- all the days fit inside 4 weeks period so we need to add two additional weeks
    -- condition is smaller than 35 because we are aiming for 42 days (6*7) and
    -- 35 is 42 - 7 and I am too lazy to check if this is leap year and february
    howManyWeeks = if (length allDays < 35) then 2 else 1
    additionalWeeks = if (length allDays < 42)
      then [(length nextMonthDays)+1..(length nextMonthDays)+(7*howManyWeeks)]
      else []
  in
    allDays ++ additionalWeeks

-- helpers
daysInMonth :: Integer -> Int -> Int
daysInMonth y m = monthLength (isLeapYear y) m

-- 0 = Monday
-- 6 = Sunday
dayOfWeek :: Day -> Int
dayOfWeek day =
  let
    (_,_,d) = toWeekDate day
  in
    d - 1