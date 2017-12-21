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


elt_subset <- function(x, i)
{
    if (length(dim(x)) <= 1) {
        x[i]
    } else {
        x[i,,drop = FALSE]
    }
}


key_format <- function(i)
{
    l <- as.list(i, flat = TRUE)
    s <- lapply(l, function(elt)
                if (is.logical(elt) || is.numeric(elt) || is.complex(elt))
                    as.character(elt)
                else paste0('"', as.character(elt), '"'))
    paste(s, collapse = ", ")
}


as_row_index <- function(x, i)
{
    n <- nrow(x)
    keys <- keys(x)
    
    if (is.list(i) && is.null(oldClass(i))) {
        if (is.null(keys)) {
            stop("cannot index rows with list when 'keys' is NULL")
        }
        sl <- key_slice(keys, i)
        rows <- sl$rows
        drop <- sl$drop
    } else {
        drop <- NULL
        r <- length(dim(i))
        if (r <= 1) {
            rows <- seq_len(n)
            if (is.numeric(i)) {
                # pass
            } else if (is.logical(i)) {
                if (length(i) != nrow(x)) {
                    stop(sprintf("index mask length (%.0f) must match number of rows (%.0f)",
                                 length(i), nrow(x)))
                }
                # pass
            } else {
                rn <- rownames(x)
                if (is.null(rn)) {
                    stop("cannot index with character with 'rownames' is NULL")
                }
                i <- as.character(i)
                names(rows) <- rn
            }
            rows <- rows[i]
        } else {
            if (is.null(keys)) {
                stop("cannot index rows with matrix when 'keys' is NULL")
            }
            rows <- key_index(keys, i)
        }

        if (anyNA(rows)) {
            j <- which(is.na(rows))[[1L]]
            lab <- key_format(if (length(dim(i)) <= 1) i[[j]] else i[j,])
            stop(sprintf("selected row entry %.0f (%s) does not exist", j, lab))
        }
    }

    list(rows = rows, drop = drop)
}


row_subset <- function(x, i)
{
    if (is.null(i)) {
        return(x)
    }

    keys <- keys(x)
    index <- as_row_index(x, i)

    rows <- index$rows
    drop <- index$drop

    if (!is.null(keys)) {
        # remove sliced keys
        if (!is.null(drop)) {
            if (all(drop)) {
                keys <- NULL
            } else {
                keys <- keys[rows, !drop, drop = FALSE]
            }
        } else {
            keys <- keys[rows, , drop = FALSE]
        }

        if (anyDuplicated(rows)) {
            # TODO: implement in C?
            n <- nrow(x)
            copy <- integer(n)
            newkey <- integer(length(rows))
            for (j in seq_along(rows)) {
                k <- rows[[j]]
                copy[[k]] <- copy[[k]] + 1L
                newkey[[j]] <- copy[[k]]
            }
            keys[[length(keys) + 1L]] <- newkey
        }
    }

    cols <- lapply(x, elt_subset, rows)
    attr(cols, "row.names") <- .set_row_names(length(rows))
    attr(cols, "keys") <- keys
    attr(cols, "class") <- attr(x, "class")

    cols
}


as_col_index <- function(x, i)
{
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

    cols
}


column_subset <- function(x, i)
{
    if (is.null(i)) {
        return(x)
    }

    i <- as_col_index(x, i)
    
    # downcast for list `[`; no elegant way to do this
    # https://stackoverflow.com/a/20639018/6233565
    rn <- attr(x, "row.names")
    keys <- attr(x, "keys")
    cl <- oldClass(x)
    class(x) <- NULL

    x <- x[i]

    attr(x, "row.names") <- rn
    attr(x, "keys") <- keys
    class(x) <- cl

    x
}


arg_index <- function(x, first, lastname)
{
    # This is a fragile. ideally we'd use match.call(), but we can't do
    # that here since we want to be able to index like x[x = 1], with 'x'
    # as a named argument. Find the first call that isn't forwarding ...
    for (n in 1L:sys.nframe()) {
        call <- sys.call(-n)
        if (!identical(call[[3L]], quote(expr=...))) {
            break
        }
    }

    # get the arguments
    args <- call[-1L]

    # stop off 'drop'/'value' argument
    narg <- length(args)
    argnames <- names(args)
    if (identical(argnames[[narg]], lastname)) {
        args <- args[-narg]
        argnames <- names(args)
    }

    # handle case when 'x' is a named argument
    if ("x" %in% argnames) {
        x <- first
    }

    # evaluate remaining arguments, replacing missing with NULL
    # https://stackoverflow.com/a/47316520/6233565
    miss <- vapply(args, identical, NA, quote(expr=))
    args[miss] <- list(NULL)
    args[[1L]] <- quote(list)
    index <- eval.parent(args, n + 1L)

    # get the call, for error messages
    n <- length(index)
    names <- names(index)

    if (!is.null(names)) {
        empty <- !nzchar(names)
        if (any(!nzchar(names)[-n])) {
            stop(simpleError("cannot mix named and unnamed row index arguments",
                             sys.call(-1L)))
        }

        if (empty[[n]]) {
            j <- index[[n]]
            names <- names[-n]
            index <- index[-n]
        } else {
            j <- NULL
        }

        # TODO: add keynames
        keys <- keys(x)
        ki <- match(names, names(keys))
        if (anyNA(ki)) {
            kj <- which(is.na(ki))[[1L]]
            stop(simpleError(sprintf("selected key \"%s\" does not exist",
                                     names[[kj]]),
                             sys.call(-1L)))
        }

        i <- vector("list", length(keys))
        i[ki] <- index
    } else if (n == 0L) {
        i <- j <- NULL
    } else if (n == 1L) {
        i <- NULL
        j <- index[[1L]]
    } else if (n == 2L) {
        i <- index[[1L]]
        j <- index[[2L]]
    } else {
        stop(simpleError("incorrect number of dimensions", sys.call(-1L)))
    }

    list(x = x, i = i, j = j)
}


# signature is ... instead of i, j, ... to allow columns named 'i' or 'j'
`[.dataset` <- function(x, ..., drop = FALSE)
{
    args <- arg_index(x, ..1, "drop")
    drop <- arg_option(drop)

    x <- args$x
    i <- args$i
    j <- args$j

    if (!is.null(j)) {
        x <- column_subset(x, j)
    }

    if (!is.null(i)) {
        x <- row_subset(x, i)
    }

    if (drop && length(x) == 1L) {
        x <- x[[1L]]
    }

    x
}


`[<-.dataset` <- function(x, i, j, value)
{
    if (!all(names(sys.call()) %in% c("", "value"))) {
        stop("named arguments are not allowed")
    }

    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    if (nargs() < 4L) { # x[] <- value or x[i] <- value
        if (missing(i)) {
            i <- NULL
        }
        replace_cols(x, i, value)
    } else if (missing(i)) { # x[,] <- value or x[,j] <- value
        if (missing(j)) {
            j <- NULL
        }
        replace_cols(x, j, value)
    } else { # x[i,] <- value or x[i,j] <- value
        if (missing(j)) {
            j <- NULL
        }
        replace_cells(x, i, j, value)
    }
}


replace_cols <- function(x, i, value)
{
    if (is.null(value)) {
        if (is.null(i)) {
            i <- seq_along(x)
        } else {
            i <- as_col_index(x, i)
        }

        # downcast to list
        cl <- class(x)
        keys <- attr(x, "keys")
        rn <- attr(x, "row.names")
        class(x) <- NULL

        x[i] <- NULL
        
        # restore
        attr(x, "row.names") <- rn
        attr(x, "keys") <- keys
        class(x) <- cl

        x
    } else {
        replace_cells(x, NULL, i, value)
    }
}


replace_cells <- function(x, i, j, value)
{
    if (is.null(i)) {
        i <- seq_len(nrow(x))
    } else {
        i <- as_row_index(x, i)$rows
    }
    if (is.null(j)) {
        j <- seq_along(x)
    } else {
        j <- as_col_index(x, j)
    }

    ni <- length(i)
    nj <- length(j)
    dv <- dim(value)
    rv <- length(dv)

    if ((asis <- class(value)[[1L]] == "AsIs")) {
        class(value) <- class(value)[-1L]
    }

    if (rv < 2L) {
        nv <- if (asis) 1L else length(value)
        if (nv == ni * nj) {
            recycle <- FALSE
        } else if (nv == ni || nv == 1L) {
            recycle <- TRUE
        } else {
            stop(sprintf("replacement has %.0f items, need %.0f", nv, ni * nj))
        }
    } else if (rv == 2L) {
        if (!(dv[[1L]] == ni && dv[[2L]] == nj)) {
            stop(sprintf("replacement has dimensions %.0fx%.0f, need %.0fx%.0f",
                         dv[[1L]], dv[[2L]], ni, nj))
        }
    } else {
        stop(sprintf("replacement cannot be a rank-%.0f array", length(dv)))
    }

    if (ni == 0 || nj == 0) {
        return(x)
    }

    if (rv < 2L) {
        if (recycle) {
            for (k in seq_along(j)) {
                jk <- j[[k]]
                if (length(dim(x[[jk]])) <= 1L) {
                    x[[jk]][i] <- value
                } else {
                    x[[jk]][i,] <- value
                }
            }
        } else {
            off <- 0L
            for (k in seq_along(j)) {
                ix <- (off + 1L):(off + ni)
                jk <- j[[k]]
                if (length(dim(x[[jk]])) <= 1L) {
                    x[[jk]][i] <- value[ix]
                } else {
                    x[[jk]][i,] <- value[ix]
                }
                off <- off + ni
            }
        }
    } else {
        for (k in seq_along(j)) {
            jk <- j[[k]]
            if (length(dim(x[[jk]])) <= 1L) {
                x[[jk]][i] <- value[, k, drop = TRUE]
            } else {
                x[[jk]][i,] <- value[, k, drop = TRUE]
            }
        }
    }

    x
}



`[[<-.dataset` <- function(x, i, value)
{
    # downcast to list
    keys <- attr(x, "keys")
    n <- .row_names_info(x, 2L)
    cl <- class(x)
    class(x) <- NULL

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

    # restore
    class(x) <- cl
    attr(x, "keys") <- keys
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
