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


key_unescape <- function(x)
{
    gsub("\\\\(\\\\|,)", "\\1", x)
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


key_decode <- function(x, composite = TRUE)
{
    if (is.null(x)) {
        NULL
    } else if (!composite) {
        list(x)
    } else {
        # split at non-escaped ','
        # https://stackoverflow.com/a/11819111/6233565
        parts <- strsplit(x, "(?<!\\\\)(?:\\\\\\\\)*,", perl = TRUE)
        len <- vapply(parts, length, 0L)
        nkey <- max(len)

        # pad with NA
        parts <- lapply(parts, `length<-`, nkey)

        # unescape and transpose
        k <- split(key_unescape(c(parts, recursive = TRUE)),
                   rep(seq_len(nkey), length(parts)))
        unname(k)
    }
}


key_index <- function(x, i, default = NA_integer_)
{
    if (is.null(i)) {
        return(NULL)
    }

    d <- dim(i)
    r <- length(d)

    if (r <= 1L) {
        ni <- length(i)
    } else if (r == 2L) {
        ni <- nrow(i)
    } else if (r > 2L) {
        stop(sprintf("cannot index with rank-%.0f array", r))
    }

    if (r == 2L) {
        if (ncol(i) != ncol(x)) {
            stop(sprintf("number of index components (%.0f) must match number of key components (%.0f)", ncol(i), ncol(x)))
        }
    }

    if (is.null(x)) {
        return(rep(default, ni))
    }

    if (r == 2L) {
        i <- key_encode(i)
    }

    n <- nrow(x)
    id <- seq_len(n)
    names(id) <- key_encode(x)
    id <- id[as.character(i)]
    id[is.na(id)] <- default
    names(id) <- NULL

    return(id)
}
