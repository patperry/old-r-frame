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
    elt <- .subset2(x, i)
    if (is.null(elt)) {
        stop(sprintf("column \"%s\" does not exist", as.character(i)))
    }
    elt
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
        if (!(n2 == n || (n2 == 1L && r < 2L))) {
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

    if (i == n1 && !is.na(name) && nzchar(name)) {
        if (is.null(names(x))) {
            names(x) <- character(n1)
        }
        names(x)[[i]] <- name
    }
    x
}


`[.dataset` <- function(x, i, j, drop = FALSE)
{
    x <- as_dataset(x)
    args <- arg_index(nargs() - 1L - !missing(drop), i, j)
    drop <- arg_option(drop)

    i <- args$i
    j <- args$j
    pairs <- args$pairs

    if (!is.null(j)) {
        x <- column_subset(x, j)
    }

    if (!is.null(i)) {
        x <- row_subset(x, i)
    }

    if (!is.null(pairs)) {
        return(get_pairs(x, pairs))
    }

    if (drop) {
        dim <- dim(x)
        if (!is.null(i) && dim[[1L]] == 1L) {
            x <- lapply(x, drop_row_dim)
        }
        if (!is.null(j) && dim[[2L]] == 1L) {
            x <- x[[1L]]
        }
    }

    x
}


drop_row_dim <- function(x)
{
    dim <- dim(x)
    if (length(dim) < 2L) {
        x[[1L]]
    } else if (is.data.frame(x)) {
        # can't use x[1L, , drop = TRUE] for data.frame since that does
        # not return a list when x has 0 or 1 column
        lapply(x, drop_row_dim)
    } else {
        x[1L, , drop = TRUE]
    }
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
    rows <- arg_row_index(x, i, call)
    keys <- keys(x)

    if (!is.null(keys)) {
        keys <- keys[rows, , drop = FALSE]

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


get_pairs <- function(x, pairs)
{
    pairs <- arg_pairs_index(x, pairs)

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

    vals <- lapply(seq_along(i), function(k) {
        jk <- j[[k]]
        if (is.na(jk)) {
            NA
        } else {
            ik <- i[[k]]
            xk <- x[[jk]]
            if (length(dim(xk)) <= 1L) {
                xk[[ik]]
            } else {
                xk[ik, , drop = TRUE]
            }
        }
    })

    vals
}


`[<-.dataset` <- function(x, i, j, value)
{
    x <- as_dataset(x)
    args <- arg_index(nargs() - 2L, i, j)
    i <- args$i
    j <- args$j
    pairs <- args$pairs

    if (is.null(value)) {
        # pass
    } else if (is.null(pairs)) {
        cl <- class(value)
        if (cl[[1L]] == "AsIs") {
            class(value) <- cl[-1L]
            value <- as_dataset(list(list(value)))
        } else {
            value <- as_dataset(value)
        }
    }

    if (!is.null(pairs)) {
        replace_pairs(x, pairs, value)
    } else if (is.null(i)) {
        replace_cols(x, j, value)
    } else {
        replace_cells(x, i, j, value)
    }
}


replace_pairs <- function(x, pairs, value, call = sys.call(-1L))
{
    pairs <- arg_pairs_index(x, pairs)

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

    if (anyNA(i) || anyNA(j)) {
        stop(simpleError("NAs are not allowed in subscripted assignments",
                         call))
    }

    n <- length(i)
    nv <- length(value)

    if (nv == 1L) {
        value <- value[[1L]]
        for (k in seq_len(n)) {
            jk <- j[[k]]
            ik <- i[[k]]
            if (length(dim(x[[jk]])) <= 1) {
                x[[jk]][[ik]] <- value
            } else {
                x[[jk]][ik, ] <- value
            }
        }
    } else if (nv == n) {
        for (k in seq_len(n)) {
            jk <- j[[k]]
            ik <- i[[k]]
            if (length(dim(x[[jk]])) <= 1) {
                x[[jk]][[ik]] <- value[[k]]
            } else {
                x[[jk]][ik, ] <- value[[k]]
            }
        }
    } else {
        stop(simpleError(sprintf("number of values (%.0f) must match number of elements to replace (%.0f)", nv, n)))
    }

    x
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


arg_index <- function(nargs, i, j, call = sys.call(-1L))
{
    if (nargs == 1L) {
        if (missing(i)) {
            return(NULL)
        } else if (is.null(i)) {
            return(list(j = integer()))
        }

        r <- length(dim(i))
        if (r <= 1L) {
            return(list(j = i))
        } else if (r == 2L) {
            return(list(pairs = i))
        } else {
            stop(simpleError(sprintf("cannot index with a rank-%.0f array",
                                     r), call))
        }
    } else if (nargs == 2L) {
        if (missing(i)) {
            i <- NULL
        } else if (is.null(i)) {
            i <- integer()
        }
        if (missing(j)) {
            j <- NULL
        } else if (is.null(j)) {
            j <- integer()
        }
        return(list(i = i, j = j))
    }

    NULL
}


arg_pairs_index <- function(x, i, call = sys.call(-1L))
{
    nr <- nrow(x)
    nc <- ncol(x)
    nel <- nr * nc

    i <- as.matrix(i)
    d <- dim(i)
    d1 <- d[[1L]]
    d2 <- d[[2L]]

    if (is.logical(i)) {
        if (d2 == 1L) {
            i <- i[, 1L, drop = TRUE]
            if (length(i) != nel) {
                if (length(i) == 1L) {
                    i <- rep(i, nel)
                } else {
                    stop(simpleError(sprintf("selection mask length (%.0f) must equal number of elements (%.0f)", length(i), nel), call))
                }
            }
        } else if (d1 == nr && d2 == nc) {
            i <- as.logical(i)
        } else {
            stop(simpleError(sprintf("selection mask dimensions (%.0f, %.0f) must match data dimensions (%.0f, %.0f)", d1, d2, nr, nc), call))
        }

        i <- seq_len(nel)[as.logical(i)]
        vec <- TRUE
    } else if (d2 == 1L) {
        i <- trunc(as.numeric(i))
        bounds <- which(!(is.na(i) | (1L <= i & i <= nel)))
        if (length(bounds) > 0L) {
            stop(simpleError(sprintf("index %.0f is out of bounds",
                                     bounds[[1L]]), call))
        }
        vec <- TRUE
    } else if (d2 == 2L) {
        row <- trunc(as.numeric(i[, 1L, drop = TRUE]))
        col <- trunc(as.numeric(i[, 2L, drop = TRUE]))

        bounds <- which(!((is.na(row) | (1L <= row & row <= nr))
                          && ((is.na(col) | (1L <= col & col <= nc)))))
        if (length(bounds) > 0L) {
            stop(simpleError(sprintf("index %.0f is out of bounds",
                                     bounds[[1L]]), call))
        }

        row[is.na(col)] <- NA
        col[is.na(row)] <- NA
        vec <- FALSE
    } else {
        stop(simpleError(sprintf("cannot index with %.0f-column matrix",
                                 d2), call))
    }

    if (vec) {
        if (nr > 0L) {
            i0 <- i - 1L
            row <- i0 %% nr + 1L
            col <- i0 %/% nr + 1L
        } else {
            row <- col <- rep(NA_integer_, length(i))
        }
    }

    cbind(row, col)
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
    
    r <- length(dim(i))
    if (is.list(i) && r <= 1L) {
        i <- as_dataset(i)
        r <- length(dim(i))
    }

    if (r <= 1L) {
        rows <- seq_len(n)
        if (is.numeric(i)) {
            # pass
        } else if (is.logical(i)) {
            if (length(i) != nrow(x)) {
                stop(simpleError(sprintf(
                     "index mask length (%.0f) must match number of rows (%.0f)",
                     length(i), nrow(x)), call))
            }
            # pass
        } else {
            rn <- rownames(x)
            if (is.null(rn)) {
                stop(simpleError(
                     "cannot index with character with 'rownames' is NULL",
                     call))
            }
            i <- as.character(i)
            names(rows) <- rn
        }

        rows <- rows[i]

    } else {
        if (is.null(keys)) {
            stop(simpleError(
                 "cannot index rows with matrix when 'keys' is NULL", call))
        }
        rows <- rowid(keys, i)
    }

    if (anyNA(rows)) {
        j <- which(is.na(rows))[[1L]]
        lab <- key_format(if (length(dim(i)) <= 1) i[[j]] else i[j,])
        stop(simpleError(sprintf(
             "selected row entry %.0f (%s) does not exist", j, lab), call))
    }

    rows
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
