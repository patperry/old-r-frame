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


option_rows <- function(rows)
{
    if (is.null(rows)) {
        rows <- getOption("frame.rows")
        if (is.null(rows)) {
            rows <- 20L
        }
    }
    rows
}


option_wrap <- function(wrap)
{
    if (is.null(wrap)) {
        wrap <- getOption("frame.wrap")
        if (is.null(wrap)) {
            wrap <- 20L
        }
    }
    wrap
}
