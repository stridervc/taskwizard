module DateParser
 ( parseDate
 ) where

import Data.Time

lowercase = id
strip = id

-- given now's time, parse string
parseDate :: ZonedTime -> String -> UTCTime
parseDate nowz str
  | s == "yesterday"  = eod $ pred day
  | s == "today"      = eod day
  | s == "tomorrow"   = eod $ succ day
  where s           = lowercase $ strip str
        now         = zonedTimeToUTC nowz
        day         = utctDay now
        zone        = zonedTimeZone nowz
        eod         = midnightOfDayToUTC zone

-- take a timezone and a day, and return midnight of that
-- day expressed in UTC
midnightOfDayToUTC :: TimeZone -> Day -> UTCTime
midnightOfDayToUTC zone day = UTCTime day' tod'
  where (daysOff, tod)  = localToUTCTimeOfDay zone midnight
        day'            = addDays daysOff day
        tod'            = timeOfDayToTime tod
