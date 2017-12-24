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

unnest <- function(x)
{
    # TODO: better error checking

    n <- length(x)
    y <- x[[1L]]

    if (is.list(y) && is.null(oldClass(y))) {
        y <- as.list(y)
        nc <- length(y)
        for (j in seq_len(nc)) {
            xj <- lapply(x, `[[`, j)
            y[[j]] <- unnest(xj)
        }
    } else if (length(y) == 1L && (is.atomic(y) || is.list(y) || isS4(y))) {
        length(y) <- n
        for (i in seq.int(2L, length.out = n - 1L)) {
            y[[i]] <- x[[i]]
        }
    } else {
        y <- x
    }

    y
}


grouped <- function(x, by = NULL, do = NULL, ...)
{
    UseMethod("grouped")
}


grouped.default <- function(x, by = NULL, do = NULL, ...)
{
    x <- as_dataset(x)
    grouped(x, by, do, ...)
}


grouped.dataset <- function(x, by = NULL, do = NULL, ...)
{
    x <- arg_dataset(x)

    if (is.null(by)) {
       # pass
    } else if (length(dim(by)) < 2L) {
        j <- arg_by_cols(x, by)
        if (length(j) > 0) {
            by <- x[j]
            x <- x[-j]
        } else {
            by <- NULL
        }
    } else {
        by <- as_dataset(by, simple = TRUE)
        if (nrow(by) != nrow(x)) {
            stop(sprintf("'by' rows (%.0f) must match data rows (%.0f)",
                         nrow(by), nrow(x)))
        }
    }

    do <- arg_function(do)

    # split into parts
    if (is.null(by)) {
        y <- as_dataset(list(list(x)))
    } else {
        keys <- unique(by, sorted = TRUE)
        g <- lookup(by, keys)
        xg <- split(x, g)

        if (length(xg) == 0L) {
            return(NULL)
        }

        y <- framed(list(xg), keys)
    }

    if (!is.null(do)) {
        # TODO: don't use lapply in case ... has arg named X or FUN
        val <- lapply(X = y[[1L]], FUN = do, ...)
        y <- framed(unnest(val), keys(y))
    }

    y
}
