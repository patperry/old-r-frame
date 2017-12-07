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


# Indexing Modes
#
# Single
# x[j] columns indicated by j
#
# Multiple
# x[cbind(k1,k2),] all columns with keys in the specified rows
# x[cbind(k1,k2),j] columns j with specified rows
# x[i,j] (numeric i) rows with index i, columns j
# x[i,j] (logical i) rows with i TRUE, columns j
# x[i,j] (vector i) rows with names as.character(i), columns j
# x[k1,,j] rows with keys in k1, columns j
# x[k1,k2,j] rows with keys in k1 x k2, columns j

# If #index == 1 (x[j]) then equivalent to x[,j].
#
# If #index == 2 (x[i,j])
#   case i is missing:
#      same as x[j]
#   case i is a matrix:
#      ncol(i) = nkey, rows identify rows in x; no dups allowed
#   case i is logical vector:
#      length(i) == nrow(x), rows identify rows in x
#   case i is numeric vector:
#      entries of i are row numbers in x; no dups allowed
#   case i other vector:
#      as_utf8(as.character(i)) is row name
#
# If #index == nkey + 1 (x[k1,k2,k3,j])
#      as_utf8(as.character(ki)) compared against keylevels(x)[[i]]
#      keep rows with first key in k1,
#                 and second key in k2,
#                 and second key in k3
#      missing ki includes all
#
# TODO: for consistency, need rownames(x) == keylevels[[1]] when nkey == 1
#

elt_subset <- function(x, i)
{
    if (length(dim(x)) <= 1) {
        x[i]
    } else {
        x[i,,drop = FALSE]
    }
}

row_subset <- function(x, i, drop = TRUE)
{
    ni <- length(i)
    if (ni < 1L || (ni == 1L && is.null(i[[1L]]))) {
        return(x)
    }

    keys <- keys(x)

    if (is.null(keys)) {
        if (ni > 1) {
            stop("cannot index with multiple row indices when 'keys' is NULL")
        }

        i <- i[[1L]]
        n <- nrow(x)
        rows <- seq_len(n)

        rows <- rows[i]
        if (anyNA(rows)) {
            j <- which(is.na(rows))[[1L]]
            stop(sprintf("row selection entry %.0f is NA", j))
        }
    } else {
        rows <- key_index(keys, i)
        keys <- keys[rows, , drop = FALSE]

        if (anyDuplicated(rows)) {
            # TODO: implement in C?
            copy <- integer(nrow(x))
            newkey <- integer(length(rows))
            for (j in seq_along(rows)) {
                k <- rows[[j]]
                copy[[k]] <- copy[[k]] + 1L
                newkey[[j]] <- copy[[k]]
            }
            keys[[length(keys) + 1L]] <- as.character(newkey)
        }
    }

    cols <- lapply(x, elt_subset, rows)
    attr(cols, "row.names") <- .set_row_names(length(rows))
    attr(cols, "keys") <- keys
    attr(cols, "class") <- attr(x, "class")

    cols
}


column_subset <- function(x, i)
{
    if (is.null(i)) {
        return(x)
    }

    n <- length(x)
    if (is.logical(i) && length(i) != n) {
        stop(sprintf(
            "selection mask length (%.0f) must equal number of columns (%.0f)",
            length(i), n))
    }

    cols <- seq_len(n)
    names(cols) <- names(x)
    cols <- cols[i]

    if (anyNA(cols)) {
        j <- which(is.na(cols))[[1L]]
        if (is.na(i[[j]])) {
            stop(sprintf("column selection entry %.0f is NA", j))
        } else if (is.character(i[[j]])) {
            stop(sprintf("selected column \"%s\" is undefined", i[[j]]))
        } else {
            stop(sprintf("column selection entry %.0f is out of bounds", j))
        }
    }

    # downcast for list `[`; no elegant way to do this
    # https://stackoverflow.com/a/20639018/6233565
    rn <- attr(x, "row.names")
    keys <- attr(x, "keys")
    cl <- oldClass(x)
    class(x) <- NULL

    x <- x[cols]

    attr(x, "row.names") <- rn
    attr(x, "keys") <- keys
    class(x) <- cl

    x
}


`[.dataset` <- function(x, ..., drop = TRUE)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }

    # https://stackoverflow.com/a/47316520/6233565
    args <- match.call()[-1]
    if (!missing(drop)) {
        args <- args[-length(args)]
    }

    miss <- vapply(args, identical, NA, quote(expr=))
    args[miss] <- list(NULL)
    args[[1L]] <- quote(list)
    index <- eval.parent(args)

    n <- length(index)

    if (n == 0L) {
        x
    } else if (n == 1L) {
        x <- column_subset(x, index[[1L]])
    } else {
        i <- index[-n]
        j <- index[[n]]
        x <- column_subset(x, j)
        x <- row_subset(x, i, drop)
    }

    x
}


`[[<-.dataset` <- function(x, i, value)
{
    cl <- oldClass(x)
    class(x) <- NULL
    n <- .row_names_info(x, 2L)

    if (!is.null(value)) {
        r <- length(dim(value))
        if (r > 2) {
            stop("replacement is not a vector or matrix")
        }
        n2 <- nrow_column(value)
        if (!(n2 == n || (n2 == 1L && r == 1L))) {
            stop(sprintf("replacement has %.0f rows, data has %.0f", n2, n))
        }
        value <- as_column(value, n)
    }

    x[[i]] <- value
    class(x) <- cl
    x
}


`$<-.dataset` <- function(x, name, value)
{
    n1 <- length(x) + 1L
    i <- match(name, names(x), n1)
    x[[i]] <- value

    if (i == n1 && !is.na(name)) {
        if (is.null(names(x))) {
            names(x) <- character(n1)
        }
        names(x)[[i]] <- name
    }
    x
}
