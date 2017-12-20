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
    as_dataset(x)
}


as_dataset <- function(x, ...)
{
    UseMethod("as_dataset")
}


as_dataset.dataset <- function(x, ...)
{
    cl <- class(x)
    i <- match("dataset", cl)
    if (i > 1L) {
        cl <- cl[-seq_len(i - 1L)]
        class(x) <- cl
    }

    x
}


as_dataset.default <- function(x, ...)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    as_dataset(x)
}


as_dataset.data.frame <- function(x, ...)
{
    # convert row names to the first column
    keys <- if (.row_names_info(x) > 0)
        keyset(name = row.names(x))
    else NULL

    l <- as.list(x)
    if (length(l) > 0L) {
        x <- as_dataset(l, ...)
    } else {
        x <- structure(l, class = c("dataset", "data.frame"),
                       row.names = .set_row_names(nrow(x)))
    }
    keys(x) <- keys
    x
}


as_dataset.list <- function(x, ...)
{
    nc <- length(x)
    names <- names(x) <- arg_names(nc, "columns", names(x), na = "")

    # make sure columns are vectors and matrices only
    # TODO: refactor
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

    structure(cols, class = c("dataset", "data.frame"),
              row.names = .set_row_names(nr))
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}
