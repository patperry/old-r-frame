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

    if (is.null(value)) {
        attr(x, "keys") <- NULL
        return(x)
    }

    n <- nrow(x)
    value <- framed(value)
    keys(value) <- NULL

    # validate key length
    if (length(value) > .Machine$integer.max) {
        stop(sprintf("number of key columns exceeds maximum (%d)",
                     .Machine$integer.max))
    }

    # validate key names, compute display string
    names <- names(value)
    if (is.null(names)) {
        nstrs <- rep("", length(names))
    } else {
        i <- which(is.na(names))
        if (length(i) > 0) {
            stop(sprintf("key column name %d is missing", i))
        }
        nstrs <- vapply(names, function(nm)
                            if (is.null(nm)) "" else sprintf(" (\"%s\")", nm), "")
    }

    # validate columns convert to UTF-8
    for (i in seq_along(value)) {
        elt <- value[[i]]
        nm <- nstrs[[i]]

        d <- dim(elt)
        if (length(d) > 1) {
            stop(sprintf("key column %d%s is not a vector", i, nm))
        }

        if (length(elt) != n) {
            stop(sprintf("key column %d%s length (%.0f) not equal to number of rows (%.0f)",
                         i, nm, length(elt), n))
        }

        j <- which(is.na(elt))
        if (length(j) > 0) {
            stop(sprintf("key column %d%s has a missing value (entry %.0f)",
                         i, nm, j[[1]]))
        }

        elt <- as.character(elt)
        j <- which(!utf8_valid(elt))
        if (length(j) > 0) {
            stop(sprintf(
                "key column %d%s cannot be converted to UTF-8 (entry %.0f is invalid)",
                i, nm, j[[1]]))
        }
        value[[i]] <- as_utf8(elt)
    }

    if (length(value) == 1) {
        i <- which(duplicated(value[[1]]))
        if (length(i) > 0) {
            j <- which(value[[1]] == value[[1]][[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("key set has duplicate entries (%.0f and %.0f)",
                         j[[1]], j[[2]]))
        }
    } else {
        kv <- key_encode(value)
        i <- which(duplicated(kv))
        if (length(i) > 0) {
            j <- which(kv == kv[[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("key set has duplicate rows (%.0f and %.0f)",
                         j[[1]], j[[2]]))
        }
    }

    # set the key
    attr(x, "keys") <- structure(value, class = c("dataset", "data.frame"),
                                 row.names = .set_row_names(n))
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
        lapply(keys, function(elt) unique(as_utf8(as.character(elt))))
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


key_junk <- function()
{
    if (is.character(i)) {
        ix <- seq_len(n)
        if (is.character(i)) {
            names(ix) <- key_encode(x)
        }
        ix <- ix[i]
        if (anyNA(ix)) {
            j <- which(is.na(ix))[[1L]]
            if (is.character(i)) {
                stop(sprintf(
                    "selected row entry %.0f (\"%s\") does not exist",
                    j, i[[j]]))
            }

            stop(sprintf("row selection entry %.0f is NA", j))
        }
    } else if (is.list(i)) {
           } else {
        stop(sprintf("invalid index type: \"%s\"", class(i)))
    }
    ix
}
