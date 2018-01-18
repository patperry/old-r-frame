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


as_dataset <- function(x, ..., simple = FALSE)
{
    UseMethod("as_dataset")
}


col_name_paren <- function(x, i)
{
    names <- names(x)
    if (is.null(names)) {
        ""
    } else if (is.na(names[[i]])) {
        " (<NA>)"
    } else {
        paste0(" (\"", names[[i]], "\")")
    }
}


as_dataset.AsIs <- function(x, ..., simple = FALSE)
{
    class(x) <- class(x)[-1L]
    as_dataset(list(x), simple = simple)
}


as_dataset.dataset <- function(x, ..., simple = FALSE)
{
    x <- arg_dataset(x)
    simple <- arg_option(simple)

    x <- downcast(x, "dataset")
    if (!simple) {
        return(x)
    }

    l <- as.list(x, flat = TRUE)
    keys <- keys(x)
    x <- structure(l, class = c("dataset", "data.frame"),
                   row.names = .set_row_names(nrow(x)))
    keys(x) <- keys

    if (length(x) > .Machine$integer.max) {
        stop(simpleError(
             sprintf("number of columns exceeds maximum (%d)",
                     .Machine$integer.max), call))
    }

    for (i in seq_along(x)) {
        x[[i]] <- as_simple_vector(x[[i]])
    }

    x
}


as_dataset.default <- function(x, ..., simple = FALSE)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    as_dataset(x, ..., simple = simple)
}


as_dataset.data.frame <- function(x, ..., simple = FALSE)
{
    simple <- arg_option(simple)

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

    if (simple) {
        x <- as_dataset(x, simple = TRUE)
    }

    x
}


as_dataset.array <- function(x, ..., simple = FALSE)
{
    dim <- dim(x)
    r <- length(dim)

    if (r > 2L) {
        stop(sprintf("cannot convert rank-%.0f array to dataset", r))
    }

    x <- as.matrix(x)
    as_dataset(x, ..., simple = simple)
}


as_dataset.matrix <- function(x, ..., simple = FALSE)
{
    names <- colnames(x)
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    names(x) <- names
    as_dataset(x, ..., simple = simple)
}


as_dataset.list <- function(x, ..., simple = FALSE)
{

    simple <- arg_option(simple)
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
    nr <- if (nc == 0L) 1L else nrow_dataset(x)
    cols <- lapply(x, as_column, nr)

    x <- structure(cols, class = c("dataset", "data.frame"),
                   row.names = .set_row_names(nr))
    if (simple) {
        x <- as_dataset(x, simple = TRUE)
    }

    x
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}


is_simple_dataset <- function(x)
{
    if (!is_dataset(x)) {
        return(FALSE)
    }

    if (length(x) > .Machine$integer.max) {
        return(FALSE)
    }

    for (i in seq_along(x)) {
        if (!is_simple_vector(x[[i]])) {
            return(FALSE)
        }
    }

    TRUE
}
