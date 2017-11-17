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

key <- function(x)
{
    UseMethod("key")
}


key.dataset <- function(x)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }
    nkey <- attr(x, "nkey")
    if (nkey == 0L) {
        NULL
    } else {
        key <- seq_len(nkey)
        attr(x, "names")[key]
    }
}


`key<-` <- function(x, value)
{
    UseMethod("key<-")
}


`key<-.dataset` <- function(x, value)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    # discard the old key
    attr(x, "nkey") <- 0L

    if (is.null(value)) {
        return(x)
    }

    names <- names(x)
    with_rethrow({
        key <- as_key("key", value, names)
    })

    # validate keys
    nk <- length(key)
    keyvals <- if (nk > 0) vector("list", nk) else NULL
    for (k in seq_along(key)) {
        elt <- x[[key[[k]]]]
        d <- dim(elt)
        if (length(d) > 1) {
            stop(sprintf("key column \"%s\" is not a vector", names[[key[[k]]]]))
        }
        i <- which(is.na(elt))
        if (length(i) > 0) {
            stop(sprintf("key column \"%s\" has a missing value (entry %.0f)",
                         names[[key[[k]]]], i[[1]]))
        }
        keyvals[[k]] <- as.character(elt)
        i <- which(!utf8_valid(keyvals[[k]]))
        if (length(i) > 0) {
            stop(sprintf(
                "key column \"%s\" cannot be converted to UTF-8 (entry %.0f is invalid)",
                names[[key[[k]]]], i[[1]]))
        }
        keyvals[[k]] <- as_utf8(keyvals[[k]], normalize = TRUE)
    }

    if (nk == 1) {
        i <- which(duplicated(keyvals[[1]]))
        if (length(i) > 0) {
            j <- which(keyvals[[1]] == keyvals[[1]][[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("key column \"%s\" has duplicate entries (%.0f and %.0f)",
                         names[[key[[1]]]], j[[1]], j[[2]]))
        }
    } else {
        kv <- key_encode(keyvals)
        i <- which(duplicated(kv))
        if (length(i) > 0) {
            j <- which(kv == kv[[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("key column set (%s) has duplicate rows (%.0f and %.0f)",
                         paste(paste0("\"", names[key], "\""), collapse = ", "),
                         j[[1]], j[[2]]))
        }
    }

    # reorder columns, putting key first
    x <- x[c(key, seq_along(x)[-key])]

    # set the key
    attr(x, "nkey") <- length(key)

    x
}


keyvals <- function(x)
{
    UseMethod("keyvals")
}


keyvals.dataset <- function(x)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    nkey <- attr(x, "nkey")
    if (nkey == 0L) {
        NULL
    } else {
        key <- seq_len(nkey)
        l <- as.list(x)
        lapply(l[key], function(elt) unique(as_utf8(as.character(elt),
                                                    normalize = TRUE)))
    }
}


key_escape <- function(x)
{
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
        x[[1]]
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
