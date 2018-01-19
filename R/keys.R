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

keys <- function(x)
{
    UseMethod("keys")
}

keys.default <- function(x)
{
    NULL
}

keys.dataset <- function(x)
{
    attr(x, "keys", TRUE)
}

`keys<-` <- function(x, value)
{
    UseMethod("keys<-")
}


`keys<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        if (!is.null(keys(x))) {
            attr(x, "keys") <- NULL
        }
    } else {
        value <- as_keyset(value)

        n <- dim(x)[[1L]]
        nk <- dim(value)[[1L]]
        if (n != nk) {
            stop(sprintf("mismatch: keys have %.0f rows, data have %.0f",
                         nk, n))
        }

        attr(x, "keys") <- value
    }

    x
}


`keys<-.keyset` <- function(x, value)
{
    stop("setting 'keys' on a keyset object is not allowed")
}


keylevels <- function(x)
{
    UseMethod("keylevels")
}


keylevels.default <- function(x)
{
    keys <- keys(x)
    if (is.null(keys)) {
        NULL
    } else {
        keylevels(keys)
    }
}


keylevels.keyset <- function(x)
{
    n <- length(x)
    levels <- vector("list", n)
    names(levels) <- names(x)

    for (i in seq_len(n)) {
        lv <- unique(x[[i]])
        o <- order(lv, method = "radix")
        levels[[i]] <- lv[o]
    }

    levels
}
