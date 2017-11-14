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

dataset <- function(..., key = NULL)
{
    qs <- quos(..., .named = TRUE)
    names <- names(qs)

    x <- list()
    for (i in seq_along(qs)) {
        nm <- names[[i]]
        y <- eval_tidy(qs[[i]], data = x)

        # safer than 'x[[nm]] <- y' since y might be NULL or nm duplicated
        x <- c(x, list(y))
        names(x)[[i]] <- nm
    }

    as_dataset(x, key)
}


as_dataset <- function(x, key = NULL, ...)
{
    UseMethod("as_dataset")
}


as_dataset.default <- function(x, key = NULL, ...)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    as_dataset(x, key, ...)
}


as_dataset.data.frame <- function(x, key = NULL, ..., rownames = "name")
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    with_rethrow({
        rownames <- as_character_scalar("rownames", rownames)
    })

    # convert row names to the first column
    if (.row_names_info(x) > 0 && !is.null(rownames)) {
        if (rownames %in% names(x)) {
            stop(sprintf("cannot create column for row names; name \"%s\" already exists", rownames))
        }
        x <- structure(c(list(as.character(rownames(x))), as.list(x)),
                       names = c(rownames, names(x)))

        if (is.null(key)) {
            key <- rownames
        }
    } else {
        x <- as.list(x)
    }

    as_dataset(x, key)
}


as_dataset.list <- function(x, key = NULL, ...)
{
    if (!is.list(x)) {
        stop("argument is not a list")
    }

    nc <- length(x)
    if (nc > .Machine$integer.max) {
        stop(sprintf("number of columns (%.0f) exceeds maximum (%d)",
                     nc, .Machine$integer.max))
    }

    with_rethrow({
        names <- as_names("column name", names(x), nc)
        key <- as_key("key", key, names)
    })
    names(x) <- names

    # make sure columns are vectors and matrices only
    for (i in seq_len(nc)) {
        elt <- x[[i]]
        if (is.null(elt)) {
            stop(sprintf("column %d (\"%s\") is NULL", i, names[[i]]))
        }
        d <- dim(elt)
        if (length(d) > 2) {
            stop(sprintf("column %d (\"%s\") has more than 2 dimensions", i,
                         names[[i]]))
        }
    }

    # validate column lengths
    nr <- nrow_dataset(x)
    x <- lapply(x, as_column, nr)

    # validate keys
    nk <- length(key)
    keyvals <- if (nk) vector("list", nk) else NULL
    for (k in key) {
        elt <- x[[k]]
        d <- dim(elt)
        if (length(d) > 1) {
            stop(sprintf("key column \"%s\" is not a vector", names[[k]]))
        }
        i <- which(is.na(elt))
        if (length(i) > 0) {
            stop(sprintf("key column \"%s\" has a missing value (entry %.0f)",
                         names[[k]], i[[1]]))
        }
        keyvals[[k]] <- as.character(elt)
        i <- which(!utf8_valid(keyvals[[k]]))
        if (length(i) > 0) {
            stop(sprintf(
                "key column \"%s\" cannot be converted to UTF-8 (entry %.0f is invalid)",
                names[[k]], i[[1]]))
        }
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
        kv <- key_join(keyvals)
        i <- which(duplicated(kv))
        if (length(i) > 0) {
            j <- which(kv == kv[[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("key column set (%s) has duplicate rows (%.0f and %.0f)",
                         paste(paste0("\"", names[key], "\""), collapse = ", "),
                         j[[1]], j[[2]]))
        }
    }

    structure(x, class = c("dataset", "data.frame"),
              key = key, row.names = .set_row_names(nr))
}



as_dataset.dataset <- function(x, ...)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }
    class(x) <- c("dataset", "data.frame")
    x
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}
