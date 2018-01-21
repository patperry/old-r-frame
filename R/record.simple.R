#  Copyright 2018 Patrick O. Perry.
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


## Simple Records

# A *simple record* is with atomic type (`NULL`, `logical`, `raw`, `integer`,
# `double`, `complex`, `character`), or else it is a list of atomic values.

is.record.simple <- function(x)
{
    if (!is.record(x))
        FALSE
    else if (is.atomic(x))
        TRUE
    else if (!is.list(x))
        FALSE
    else {
        for (i in seq_along(x)) {
            if (!is.atomic(x[[i]]))
                return(FALSE)
        }
        TRUE
    }
}

# We can convert non-atomic vectors to simple once by recursively converting
# their components to simple, then concatenating.

as.record.simple <- function(x)
{
    if (is.record.simple(x))
        return(x)

    x <- as.record(x)
    x <- map(x, as.record.simple)
    c.record(x)
}
