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

keys.default <- function(x) NULL

keys.dataset <- function(x)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }
    attr(x, "keys")
}

`keys<-` <- function(x, value)
{
    UseMethod("keys<-")
}


`keys<-.dataset` <- function(x, value)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    with_rethrow({
        value <- as_keys("keys", value, x)
    })

    attr(x, "keys") <- value
    x
}


keylevels <- function(x)
{
    UseMethod("keylevels")
}


keylevels.dataset <- function(x)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    keys <- keys(x)
    if (is.null(keys)) {
        NULL
    } else {
        lapply(keys, function(elt) unique(elt))
    }
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


key_index <- function(x, i)
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
        return(rep(NA_integer_, ni))
    }

    if (r == 2L) {
        i <- key_encode(i)
    }

    n <- nrow(x)
    id <- seq_len(n)
    names(id) <- key_encode(x)
    id <- id[as.character(i)]
    names(id) <- NULL

    return(id)
}


key_slice <- function(x, i)
{
    if (length(x) != length(i)) {
        stop(sprintf("number of index components (%.0f) must match number of key components (%.0f)", length(i), length(x)))
    }

    n <- nrow(x)
    mask <- rep(TRUE, n)
    for (k in seq_along(i)) {
        ik <- i[[k]]
        if (is.null(ik)) {
            next
        }
        xk <- x[[k]]
        if (is.integer(xk)) {
            ik <- as.integer(ik)
        } else if (is.numeric(xk)) {
            ik <- as.numeric(ik)
        } else {
            ik <- as_utf8(as.character(ik))
        }
        mask <- mask & (xk %in% ik)
    }
    ix <- which(mask)
}
