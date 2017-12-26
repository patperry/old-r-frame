#  Copyright 2017 Patrick O. Perry.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.


# POSIXct: internally stored as numeric, number of seconds since 1970-01-01 UTC
#          "tzone" attribute stores time zone
#
# as.POSIXct.POSIXct(, tz) ignores tz
# as.POSIXlt.POSIXct(, tz) keeps time, changes to new zone
# as.Date(, tz) keeps time, changes to new zone, then gets date
#
# POSIXlt: list with year/month/day/etc components, along with time zone
#          also has tzone attribute with zone
#
# as.POSIXct.POSIXlt(, tz) changes to new time; (e.g. 6pmEST -> 6pmPST)
# as.POSIXlt.POSIXlt(, tz) ignores tz
# as.Date(, tz) ignores tz, uses y/m/d
#
# Date: internally stored as numeric, number of days since 1970-01-01 UTC
#


as_date <- function(x)
{
    tz <- get_tzone(x)
    x <- as.Date(x, tz = tz, origin = "1970-01-01")
    structure(as.numeric(x), class = "Date")
}


as_posixct <- function(x)
{
    tz <- get_tzone(x)
    x <- as.POSIXct(x, tz, origin = "1970-01-01")
    structure(as.numeric(x), class = c("POSIXct", "POSIXt"),
              tzone = attr(x, "tzone"))
}


get_tzone <- function(x)
{
    tz <- attr(x, "tzone")[[1L]]
    if (is.null(tz)) {
        tz <- "UTC"
    }
    tz
}
