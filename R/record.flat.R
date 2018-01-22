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


## Flat Records

# A *flat record* is with atomic type (`NULL`, `logical`, `raw`, `integer`,
# `double`, `complex`, `character`), or else it is a vector of atomic values.

is.record.flat <- function(x)
{
    if (!is.record(x))
        FALSE
    else if (is.atomic(x))
        TRUE
    else {
        for (i in seq_along(x)) {
            if (!is.atomic(x[[i]]))
                return(FALSE)
        }
        TRUE
    }
}

# We can convert non-atomic vectors to flat ones by recursively converting
# their components to flat, then concatenating.

as.record.flat <- function(x)
{
    if (is.atomic(x))
        return(as.record(x))

    x <- map(x, as.record.flat, named = named)
    names <- names(x)

    if (!is.null(names)) {
        for (i in seq_along(x)) {
            xi <- x[[i]]
            n <- length(x)

            if (n == 0)
                next

            prefix <- names[[i]]
            if (is.na(prefix) || !nzchar(prefix))
                next

            suffix <- names(xi)
            if (is.null(suffix))
                suffix <- as.character(seq_len(n))

            names(xi) <- paste(prefix, suffix, sep = ".")
            x[[i]] <- xi
        }
    }

    as.record(x)
}
