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

    if (!is.null(do) && nrow(y) > 0L) {
        rows <- lapply(y[[1L]], function(g) {
            row <- do(g, ...)
            if (!is.null(row)) {
                row <- as_dataset(row)
            }
            row
        })
        nr <- vapply(rows, NROW, 0)
        if (!all(nr == nr[[1L]])) {
            j <- which(nr != nr[[1L]])[[1L]]
            stop(sprintf("'do' action returned differing number of rows (%.0f, %.0f) for groups %.0f, %.0f (%s, %s)",
                             nrow(rows[[1]]), nrow(rows[[j]]),
                             1, j,
                             keys(y)[1,], keys(y)[j,]))
        }
        nr <- nr[[1L]]
        if (nr == 0) {
            y[] <- NULL
        } else if (nr == 1L) {
            cols <- do.call(rbind.dataset, rows)
            keys(cols) <- keys(y)
            y <- cols
        } else {
            stop(sprintf("'do' action returned multiple rows (%.0f)", nr))
        }
    }

    y
}
