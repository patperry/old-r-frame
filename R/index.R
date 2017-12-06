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

row_subset <- function(x, i)
{
    if (is.null(i) || (length(i) == 1L && is.null(i[[1L]]))) {
        return(x)
    }

    keys <- keys(x)

    if (is.null(keys)) {
        n <- nrow(x)
        ix <- seq_len(n)
        i <- ix[i[[1L]]]
    } else {
        i <- key_index(keys, i)
        keys <- lapply(as.list(keys), `[`, i)

        if (anyDuplicated(i)) {
            # TODO: implement in C?
            copy <- numeric(nrow(x))
            newkey <- numeric(length(i))
            for (j in seq_along(i)) {
                k <- i[[j]]
                copy[[k]] <- copy[[k]] + 1
                newkey[[j]] <- copy[[k]]
            }
            keys[[length(keys) + 1L]] <- newkey
        }
    }

    cols <- lapply(as.list(x), elt_subset, i)
    x <- as_dataset(cols)
    if (!is.null(keys)) {
        keys(x) <- as_dataset(keys)
    }

    x
}

column_subset <- function(x, i)
{
    if (is.null(i)) {
        return(x)
    }

    n <- length(x)
    if (is.logical(i)) {
        if (length(i) != n) {
            stop(sprintf(
              "logical subscript length (%.0f) must equal number of column (%d)",
              length(i), n))
        }
        if (anyNA(i)) {
            stop(sprintf("logical subscript entry %.0f is NA",
                         which(is.na(i))[[1]]))
        }
        i <- seq_len(n)[i]
    }

    if (is.numeric(i)) {
        na <- which(!is.finite(i))
        if (length(na) > 0) {
            stop(sprintf("subscript entry %.0f is %s", na[[1]], i[[na[[1]]]]))
        }

        i <- trunc(i)
        if (any(i < 0)) {
            if (any(i > 0)) {
                 stop("subscript contains both positive and with negative values")
            }
            big <- which(i < -n)
            if (length(big) > 0L) {
                stop(sprintf("subscript entry %.0f is out of bounds", big[[1]]))
            }
            i <- seq_len(n)[i]
        } else {
            big <- which(i > n)
            if (length(big) > 0L) {
                stop(sprintf("subscript entry %.0f is out of bounds", big[[1]]))
            }
            i <- i[i != 0]
        }

        # downcast for list `[`; no elegant way to do this
        # https://stackoverflow.com/a/20639018/6233565
        rn <- attr(x, "row.names")
        keys <- attr(x, "keys")
        cl <- class(x)
        class(x) <- NULL

        x <- x[i]

        attr(x, "row.names") <- rn
        attr(x, "keys") <- keys
        class(x) <- cl
    }

    # TODO handle non-logical, non-numeric

    x
}



`[.dataset` <- function(x, ..., drop = FALSE)
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
        x <- row_subset(x, i)
    }

    # TODO: handle 'drop = TRUE'
    x
}
