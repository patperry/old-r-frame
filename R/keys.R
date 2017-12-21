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
    attr(x, "keys")
}

`keys<-` <- function(x, value)
{
    UseMethod("keys<-")
}


`keys<-.dataset` <- function(x, value)
{
    if (!is.null(value)) {
        value <- as_keyset(value)

        n <- dim(x)[[1L]]
        nk <- nrow(value)
        if (n != nk) {
            stop(sprintf("key rows (%.0f) must match data rows (%.0f)", nk, n))
        }
    }

    attr(x, "keys") <- value
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
    lapply(x, unique)
}


key_escape <- function(x)
{
    x <- as_utf8(as.character(x))
    # replace '\' with '\\', ',' with '\,'
    gsub("(\\\\|,)", "\\\\\\1", x)
}


key_encode <- function(x)
{
    nk <- length(x)
    if (nk == 0) {
        NULL
    } else if (nk == 1) {
        as_utf8(as.character(x[[1]]))
    } else {
        do.call(paste, c(unname(lapply(x, key_escape)), sep = ","))
    }
}


key_index <- function(x, i, default = NA_integer_)
{
    id <- seq_len(nrow(x))
    names(id) <- key_encode(x)

    id <- id[key_encode(i)]
    id[is.na(id)] <- default

    unname(id)
}
