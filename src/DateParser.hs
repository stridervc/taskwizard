module DateParser
 ( parseDate
 ) where

import Data.Time

lowercase = id
strip = id

-- given now's time, parse string
parseDate :: ZonedTime -> String -> UTCTime
parseDate nowz str
  | s == "yesterday"  = eod yesterday
  | s == "today"      = eod today
  | s == "tomorrow"   = eod tomorrow
  where s           = lowercase $ strip str
        now         = zonedTimeToUTC nowz
        zone        = zonedTimeZone nowz
        eod         = midnightOfDayToUTC zone
        today       = utctDay now
        yesterday   = pred today
        tomorrow    = succ today

-- take a timezone and a day, and return midnight of that
-- day expressed in UTC
midnightOfDayToUTC :: TimeZone -> Day -> UTCTime
midnightOfDayToUTC zone day = UTCTime day' tod'
  where (daysOff, tod)  = localToUTCTimeOfDay zone midnight'
        day'            = addDays daysOff day
        tod'            = timeOfDayToTime tod
        midnight'       = TimeOfDay 23 59 59
