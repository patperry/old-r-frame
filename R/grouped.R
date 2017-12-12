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
    } else if (length(y) == 1L) {
        cl <- class(y)
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
    if (is.null(x)) {
        return(x)
    }

    x <- framed(x)

    # split into parts
    if (is.null(by)) {
        y <- framed(list(list(x)))
    } else {
        by <- framed(by)
        g <- key_encode(by)
        xg <- split(x, g)

        if (length(xg) == 0L) {
            return(NULL)
        }

        nm <- names(xg)

        keys <- key_decode(nm, composite = length(by) > 1L)
        for (i in seq_along(keys)) {
            storage.mode(keys[[i]]) <- storage.mode(by[[i]])
        }
        names(keys) <- names(by)
        y <- framed(list(xg), framed(keys))
    }

    if (!is.null(do)) {
        # TODO: don't use lapply in case ... has arg named X or FUN
        val <- lapply(X = y[[1L]], FUN = do, ...)
        y <- framed(unnest(val), keys(y))
    }

    y
}
