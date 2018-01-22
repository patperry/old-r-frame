#  Copyright 2018 Patrick O. Perry.
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


## Records

# A *record* is a vector of 0 or more components (or `NULL`), optionally
# with names. Duplicate and missing names are allowed.

record <- function(...)
{
    args <- list(substitute(list(...)))[-1]

    x <- list(...)
    n <- length(x)
    names <- names(x)
    if (is.null(names))
        names <- character(n)

    for (i in seq_len(n)) {
        if (!nzchar(names[[i]])) {
            names[[i]] <- deparse(args[[i]], 500)[[1]]
        }
    }

    names(x) <- names
    do(c.record, x)
}


# The underlying storage mode for `x` can be arbitrary. Records with
# components of all the same type can be stored as atomic vectors, but
# they can also be stored as lists.

is.record <- function(x, mode = "any")
    inherits(x, "record")

# We coerce other objects to records by first converting to vector. We
# preserve object names for vector inputs.

as.record <- function(x, mode = "any")
    UseMethod("as.record")

as.record.default <- function(x, mode = "any")
{
    if (is.record(x))
        return(x)

    dim <- dim(x)
    if (!is.null(dim)) {
        rank <- length(dim)
        if (rank == 2) {
            stop("cannot convert matrix to record")
        } else if (rank > 2) {
            stop(sprintf("cannot convert rank-%.0f array to record", rank))
        }
    }

    names <- names(x)
    x <- as.vector(x, mode)
    names(x) <- names
    as.record.vector(x)
}


as.record.vector <- function(x, mode = "any")
{
    if (is.record(x, mode)) {
        x
    } else if (mode == "character") {
        as.character.record(x)
    } else {
        names <- names(x)
        x <- as.vector(x, mode)
        attributes(x) <- NULL
        class(x) <- "record"
        names(x) <- names
        x
    }
}


# Records use the empty string instead of `NA` when coercing to character.

as.character.record <- function(x)
{
    record <- is.record(x)
    if (record && is.character(x))
        return(x)

    names <- names(x)
    missing <- anyNA(x)
    if (missing)
        which <- is.na(x)

    if (record)
        attributes(x) <- NULL

    x <- as.character(x)
    if (missing)
        x[which] <- NA

    class(x) <- "record"
    names(x) <- names

    x
}

as.list.record <- function(x) as.record.vector(x, "list")
as.raw.record <- function(x) as.record.vector(x, "raw")
as.logical.record <- function(x) as.record.vector(x, "logical")
as.integer.record <- function(x) as.record.vector(x, "integer")
as.double.record <- function(x) as.record.vector(x, "double")
as.complex.record <- function(x) as.record.vector(x, "complex")

as.numeric.record <- function(x)
{
    if (is.numeric(x) && is.record(x))
        x
    else
        as.double.record(x)
}

as.data.frame.record <- function(x, row.names = NULL, optional = FALSE, ...)
{
    x <- as.list.record(x)
    class(x) <- NULL
    as.data.frame(x)
}

# Records mostly behave like R base vectors, but they treat the empty
# character string `""` as a missing value, and they have slightly
# different indexing (`[`) behavior.
#
# See below for a description of the indexing behavior.

anyNA.record <- function(x, recursive = FALSE)
{
    if (!is.character(x))
        NextMethod("anyNA")

    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.na(elt) || elt == "")
            return(TRUE)
    }
    
    FALSE
}

is.na.record <- function(x)
{
    if (!is.character(x))
        NextMethod("is.na")

    n <- length(x)
    result <- logical(n)

    for (i in seq_len(n)) {
        elt <- x[[i]]
        if (is.na(elt) || elt == "")
            result[[i]] <- TRUE
    }

    result
}


as.vector.record <- function(x, mode = "any")
{
    if (mode == "character")
        as.character.record(x)
    else NextMethod("as.vector")
}



# Record names are themselves records, but unlike most R objects, they
# are required to be encoded in normalized UTF-8, and they cannot contain NA.
# They are, however, allowed to contain the empty string (`""`).
#
# Note that `names(x)` may also contain duplicate values.

names.record <- function(x)
    attr(x, "names_", TRUE)

`names<-.record` <- function(x, value)
{
    if (identical(names(x), value))
        return(x)

    if (!is.null(value)) {
        value <- as.character.record.normal(value)
        n <- length(x)
        nvalue <- length(value)

        if (nvalue != n) {
            fmt <- "mismatch: `value` length is %.0f, argument length is %.0f"
            stop(sprintf(fmt, name, nvalue, n))
        }
    }

    attr(x, "names_") <- value
    x
}


c.record <- function(...)
{
    args <- list(...)
    narg <- length(args)
    mode <- modes_combine(args)
    as.record(args, mode)
}


modes_combine <- function(xlist)
{
    n <- length(xlist)
    mode <- "NULL"

    for (i in seq_len(n)) {
        x <- xlist[[i]]
        type <- typeof(x)
        if (mode == "NULL") {
            mode <- type
        } else if (type != mode) {
            return("list")
        }
    }

    if (mode == "NULL")
        "any"
    else mode
}


    qualify_names <- function(n, names = NULL, prefix = NULL)
{
    if (is.null(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}


flatten_names <- function(nlist, nameslist = NULL, prefixlist = NULL)
{
    result <- NULL

    off <- 0
    for (i in seq_along(nlist)) {
        n <- nlist[[i]]
        names <- qualify_names(n, nameslist[[i]], prefixlist[[i]])

        if (!is.null(names)) {
            if (is.null(result))
                result <- character(sum(nlist))

            if (n > 0)
                result[(off + 1):(off + n)] <- names
        }

        off <- off + n
    }

    result
}



