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


restrict <- function(...)
{
    if (nargs() == 0L) {
        stop("missing first argument")
    }

    # TODO: pairlist?
    args <- list(...)
    x <- args[[1L]]
    slice <- args[-1L]
    restrict_slice(x, slice, drop = TRUE)
}


restrict_slice <- function(x, slice, ..., drop = FALSE)
{
    UseMethod("restrict_slice")
}


restrict_slice.default <- function(x, slice, ..., drop = FALSE)
{
    keys <- keys(x)
    nkey <- length(keys)
    keynames <- names(keys)

    slice <- arg_slice(nkey, keynames, slice)
    drop <- arg_option(drop)

    n <- length(slice)
    if (n == 0L) {
        if (drop && !is.null(keys) && nkey == 0L) {
            keys(x) <- NULL
        }
        return(x)
    }

    rows <- keyrows(keys, slice)

    dropped <- FALSE
    if (drop) {
        keep <- rep(TRUE, length(keys))
        for (i in seq_len(n)) {
            s <- slice[[i]]
            if (length(s) == 1L && class(s)[[1L]] != "AsIs") {
                keep[[i]] <- FALSE
                dropped <- TRUE
            }
        }
    }

    if (dropped) {
        keys <- keys[rows, keep]
        if (length(keys) == 0L) {
            keys <- NULL
        }
        keys(x) <- NULL
        x <- x[rows, ]
        keys(x) <- keys
    } else {
        x <- x[rows, ]
    }
}
