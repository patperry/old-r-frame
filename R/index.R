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


`[[.dataset` <- function(x, i, exact = TRUE)
{
    if (!identical(exact, TRUE)) {
        warning("'exact' argument is ignored")
    }

    # no fancy subscripting here; maybe add support for that later
    .subset2(x, i)
}


`[[<-.dataset` <- function(x, i, value)
{
    # downcast to dataset, then list
    x <- as_dataset(x)
    keys <- attr(x, "keys")
    n <- .row_names_info(x, 2L)
    cl <- class(x)
    class(x) <- NULL

    if (!is.null(value)) {
        r <- length(dim(value))
        if (r > 2L) {
            stop("replacement is not a vector or matrix")
        }
        n2 <- nrow_column(value)
        if (!(n2 == n || (n2 == 1L && r == 1L))) {
            stop(sprintf("replacement has %.0f rows, data has %.0f", n2, n))
        }
        value <- as_column(value, n)
    }

    x[[i]] <- value

    # restore attributes
    class(x) <- cl
    attr(x, "keys") <- keys
    x
}


`$.dataset` <- function(x, name)
{
    # NOTE: partial matching on name is not allowed
    x[[name]]
}


`$<-.dataset` <- function(x, name, value)
{
    n1 <- length(x) + 1L
    i <- match(name, names(x), n1)
    x[[i]] <- value

    if (i == n1 && !is.na(name) && !nzchar(name)) {
        if (is.null(names(x))) {
            names(x) <- character(n1)
        }
        names(x)[[i]] <- name
    }
    x
}


`[.dataset` <- function(x, ..., drop = FALSE)
{
    args <- arg_index(x, ..1, "drop")
    drop <- arg_option(drop)

    x <- as_dataset(args$x)
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


column_subset <- function(x, i, call = sys.call(-1L))
{
    i <- arg_col_index(x, i, call)
    
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


row_subset <- function(x, i, call = sys.call(-1L))
{
    keys <- keys(x)
    rows <- arg_row_index(x, i, call)

    if (is.list(i) && is.null(oldClass(i))) {
        drop <- vapply(i, function(ik)
                       length(ik) == 1L && class(ik)[[1L]] != "AsIs", NA)
    } else {
        drop <- NULL
    }

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
            keys <- append_copy_num(keys, nrow(x), rows)
        }
    }

    # NOTE: result is a dataset
    cols <- lapply(x, elt_subset, rows)
    attr(cols, "row.names") <- .set_row_names(length(rows))
    attr(cols, "class") <- class(x)
    keys(cols) <- keys

    cols
}


elt_subset <- function(x, i)
{
    if (length(dim(x)) <= 1L) {
        x[i]
    } else {
        x[i, , drop = FALSE]
    }
}



`[<-.dataset` <- function(x, ..., value)
{
    args <- arg_index(x, ..1, "value")
    x <- args$x
    i <- args$i
    j <- args$j
    x <- as_dataset(x)

    if (is.null(value)) {
        # pass
    } else {
        cl <- class(value)
        if (cl[[1L]] == "AsIs") {
            class(value) <- cl[-1L]
            value <- as_dataset(list(list(value)))
        } else {
            value <- as_dataset(value)
        }
    }

    if (is.null(i)) {
        replace_cols(x, j, value)
    } else {
        replace_cells(x, i, j, value)
    }
}


replace_cols <- function(x, j, value, call = sys.call(-1L))
{
    if (is.null(value)) {
        if (is.null(j)) {
            j <- seq_along(x)
        } else {
            j <- arg_col_index(x, j, call)
        }

        # downcast to list
        cl <- class(x)
        keys <- attr(x, "keys")
        rn <- attr(x, "row.names")
        class(x) <- NULL

        x[j] <- NULL
        
        # restore
        attr(x, "row.names") <- rn
        attr(x, "keys") <- keys
        class(x) <- cl
    } else if (is.character(j)) {
        if (anyNA(j)) {
            stop(simpleError("column index contains NA", call))
        }

        n <- nrow(x)
        recycle <- arg_recycle(n, length(j), value, call)
        rc <- recycle$cols
        rr <- recycle$rows

        for (k in seq_along(j)) {
            jk <- j[[k]]
            vk <- if (rc) value[[1L]] else value[[k]]
            if (rr) {
                # TODO: handle lists, S3 objects
                vk <- rep(vk, n)
            }
            x[[jk]] <- vk
        }
    } else {
        x <- replace_cells(x, NULL, j, value, call)
    }

    x
}


replace_cells <- function(x, i, j, value, call = sys.call(-1L))
{
    if (is.null(i)) {
        i <- seq_len(nrow(x))
    } else {
        i <- arg_row_index(x, i, call)
    }
    if (is.null(j)) {
        j <- seq_along(x)
    } else {
        j <- arg_col_index(x, j, call)
    }

    ni <- length(i)
    nj <- length(j)
    recycle <- arg_recycle(ni, nj, value, call)$cols

    if (ni == 0 || nj == 0) {
        return(x)
    }

    for (k in seq_along(j)) {
        jk <- j[[k]]
        vk <- if (recycle) value[[1L]] else value[[k]]

        if (length(dim(x[[jk]])) <= 1L) {
            x[[jk]][i] <- vk
        } else {
            x[[jk]][i,] <- vk
        }
    }

    x
}


arg_recycle <- function(ni, nj, value, call = sys.call(-1L))
{
    rows <- nrow(value)
    cols <- ncol(value)

    if (rows == ni) {
        recycle_rows <- FALSE
    } else if (rows == 1L) {
        recycle_rows <- TRUE
    } else {
        stop(simpleError(sprintf(
             "replacement has %.0f rows, need %.0f", rows, ni), call))
    }

    if (cols == nj) {
        recycle_cols <- FALSE
    } else if (cols == 1L) {
        recycle_cols <- TRUE
    } else {
        stop(simpleError(sprintf(
             "replacement has %.0f columns, need %.0f", cols, nj), call))
    }

    list(rows = recycle_rows, cols = recycle_cols)
}


# arg_index(x, ..1, "drop") or arg_index(x, ..1, "value")
# for x[...,drop] or x[...] <- value
arg_index <- function(x, first, lastname, call = sys.call(-1L))
{
    # This is a fragile. ideally we'd use match.call(), but we can't do
    # that here since we want to be able to index like x[x = 1], with 'x'
    # as a named argument. Find the first call that isn't forwarding ...
    for (n in 1L:sys.nframe()) {
        pcall <- sys.call(-n)
        if (!identical(pcall[[3L]], quote(expr=...))) {
            break
        }
    }

    # get the arguments
    args <- pcall[-1L]

    # split off 'drop'/'value' argument
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
        # x[name=1,] and x[name=1] are both allowed
        empty <- !nzchar(names)
        if (empty[[n]]) {
            j <- index[[n]]
            index <- index[-n]
        } else {
            j <- NULL
        }
        i <- arg_slice(keys(x), index, call)
    } else if (n == 0L) {
        i <- j <- NULL
    } else if (n == 1L) {
        i <- NULL
        j <- index[[1L]]
    } else if (n == 2L) {
        i <- index[[1L]]
        j <- index[[2L]]
    } else {
        stop(simpleError("incorrect number of dimensions", call))
    }

    list(x = x, i = i, j = j)
}


arg_col_index <- function(x, i, call = sys.call(-1L))
{
    n <- length(x)
    if (is.logical(i) && length(i) != n) {
        if (length(i) == 1L) {
            # recycle scalar (needed for 'subset.data.frame')
            i <- rep(i, n)
        } else {
            stop(simpleError(sprintf("selection mask length (%.0f) must equal number of columns (%.0f)",
                                     length(i), n), call))
        }
    }

    cols <- seq_len(n)
    names(cols) <- names(x)
    cols <- cols[i]

    if (anyNA(cols)) {
        j <- which(is.na(cols))[[1L]]
        if (is.na(i[[j]])) {
            stop(simpleError(sprintf("column selection entry %.0f is NA", j), call))
        } else if (is.character(i[[j]])) {
            stop(simpleError(sprintf("selected column \"%s\" is undefined", i[[j]]), call))
        } else {
            stop(simpleError(sprintf("column selection entry %.0f is out of bounds", j), call))
        }
    }

    cols
}


arg_row_index <- function(x, i, call = sys.call(-1L))
{
    n <- nrow(x)
    keys <- keys(x)
    
    if (is.list(i) && is.null(oldClass(i))) {
        if (is.null(keys)) {
            stop(simpleError("cannot index rows with list when 'keys' is NULL", call))
        }
        rows <- keyslice(keys, i)
    } else {
        r <- length(dim(i))
        if (r <= 1) {
            rows <- seq_len(n)
            if (is.numeric(i)) {
                # pass
            } else if (is.logical(i)) {
                if (length(i) != nrow(x)) {
                    stop(simpleError(sprintf("index mask length (%.0f) must match number of rows (%.0f)",
                                             length(i), nrow(x)), call))
                }
                # pass
            } else {
                rn <- rownames(x)
                if (is.null(rn)) {
                    stop(simpleError("cannot index with character with 'rownames' is NULL", call))
                }
                i <- as.character(i)
                names(rows) <- rn
            }
            rows <- rows[i]
        } else {
            if (is.null(keys)) {
                stop(simpleError("cannot index rows with matrix when 'keys' is NULL", call))
            }
            rows <- rowid(keys, i)
        }

        if (anyNA(rows)) {
            j <- which(is.na(rows))[[1L]]
            lab <- key_format(if (length(dim(i)) <= 1) i[[j]] else i[j,])
            stop(simpleError(sprintf("selected row entry %.0f (%s) does not exist", j, lab), call))
        }
    }

    rows
}


arg_slice <- function(keys, args, call = sys.call(-1))
{
    n <- length(keys)
    names <- names(args)
    if (is.null(names)) {
        if (length(args) > 1L) {
            stop(simpleError("only one unnamed slice argument is allowed", call))
        }
        i <- args[[1L]]
        if (is.null(args)) {
            return(NULL)
        }
        i <- as.list(i)
        if (length(i) != n) {
            stop(simpleError(sprintf("number of index components (%.0f) must match number of key components (%.0f)", length(i), n), call))
        }
    } else {
        empty <- !nzchar(names)
        if (any(empty)) {
            stop(simpleError("cannot mix named and unnamed slice arguments", call))
        }

        ki <- match(names, names(keys))
        if (anyNA(ki)) {
            kj <- which(is.na(ki))[[1L]]
            stop(simpleError(sprintf("selected key \"%s\" does not exist", names[[kj]]),
                             call))
        }

        i <- vector("list", n)
        i[ki] <- args
    }

    # convert types, keeping "AsIs" marker if present
    for (k in seq_len(n)) {
        ik <- i[[k]]

        if (is.null(ik)) {
            next
        }

        if (length(dim(ik)) > 1L) {
            stop(simpleError(sprintf("cannot slice with rank-%.0f array for key value",
                                     length(dim(ik))), call))
        }
    }

    i
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
