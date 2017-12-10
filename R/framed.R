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

dataset <- function(...)
{
    args <- as.list(substitute(list(...)))[-1L]
    x <- list(...)
    n <- length(x)

    names <- names(x)
    if (is.null(names)) {
        names <- character(n)
    }
    for (i in seq_len(n)) {
        if (names[[i]] == "") {
            name <- deparse(args[[i]], nlines = 1L)[1L]
            names[[i]] <- sub("^I\\((.*)\\)$", "\\1", name)
        }
    }
    names(x) <- names
    framed(x)
}


framed <- function(x, keys = NULL, ...)
{
    UseMethod("framed")
}


framed.default <- function(x, keys = NULL, ...)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    framed(x, keys, ...)
}


framed.data.frame <- function(x, keys = NULL, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    # convert row names to the first column
    if (is.null(keys) && .row_names_info(x) > 0) {
        keys <- dataset(name = row.names(x))
    }

    l <- as.list(x)
    framed(l, keys, ...)
}


framed.list <- function(x, keys = NULL, ...)
{
    if (!is.list(x)) {
        stop("argument is not a list")
    }

    nc <- length(x)
    with_rethrow({
        names <- as_names("column name", names(x), nc)
    })
    names(x) <- names

    # make sure columns are vectors and matrices only
    for (i in seq_len(nc)) {
        elt <- x[[i]]
        lab <- if (is.null(names)) "" else sprintf(" (\"%s\")", names[[i]])
        if (is.null(elt)) {
            stop(sprintf("column %.0f%s is NULL", i, lab))
        }
        d <- dim(elt)
        if (length(d) > 2) {
            stop(sprintf("column %.0f%s has more than 2 dimensions", i, lab))
        }
    }

    # validate column lengths
    nr <- nrow_dataset(x)
    cols <- lapply(x, as_column, nr)
    if (!is.null(keys) && length(dim(keys)) < 2L) {
        j <- as_keys("keys", keys, names)
        if (length(j) > 0) {
            keys <- framed(cols[j])
            cols <- cols[-j]
        } else {
            keys <- NULL
        }
    }

    x <- structure(cols, class = c("dataset", "data.frame"),
                   row.names = .set_row_names(nr))

    # set keys
    if (!is.null(keys)) {
        keys(x) <- keys
    }

    x
}


framed.dataset <- function(x, keys = NULL, ...)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }
    if (is.null(keys)) {
        class(x) <- c("dataset", "data.frame")
    } else {
        l <- as.list(x)
        x <- as.dataset(l, keys, ...)
    }
    x
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}
