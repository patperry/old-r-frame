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

cbind.dataset <- function(..., deparse.level = 1)
{
    # ignore 'deparse.level' argument

    x <- list(...)
    null <- vapply(x, is.null, NA)
    x <- lapply(x[!null], as_dataset)
    n <- length(x)

    if (n == 0) {
        return(NULL)
    }

    # fix names
    argnames <- names(x)
    if (!is.null(argnames)) {
        for (i in seq_len(n)) {
            nm <- argnames[[i]]
            if (nzchar(nm)) {
                xi <- x[[i]]
                if (length(xi) == 1) {
                    names(xi) <- nm
                    x[[i]] <- xi
                }
            }
        }
    }

    # get rows
    nr <- dim(x[[1L]])[[1L]]

    # validate rows
    for (i in seq_len(n)) {
        xi <- x[[i]]
        if (dim(xi)[[1L]] != nr) {
            index <- which(!null)
            stop(sprintf("arguments %.0f and %.0f have different numbers of rows",
                         index[[1]], index[[i]]))
        }
    }

    # get keys
    for (ikey in seq_len(n)) {
        keys <- keys(x[[ikey]])
        if (!is.null(keys)) {
            break
        }
    }

    # validate keys
    if (ikey < n) {
        for (i in (ikey + 1L):n) {
            xi <- x[[i]]
            ki <- keys(xi)
            if (!is.null(ki) && !identical(ki, keys)) {
                index <- which(!null)
                stop(sprintf("arguments %.0f and %.0f have different keys",
                             index[[ikey]], index[[i]]))
            }
        }
    }

    nctot <- sum(vapply(x, ncol, 0))
    if (nctot == 0) {
        x1 <- x[[1]]
        keys(x1) <- keys
        return(x1)
    }

    y <- vector("list", nctot)
    names <- NULL

    off <- 0L
    for (i in seq_along(x)) {
        xi <- x[[i]]
        nc <- ncol(xi)

        ix <- off + seq_len(nc)
        y[ix] <- xi

        ni <- names(xi)
        if (!is.null(ni)) {
            if (is.null(names)) {
                names <- character(nctot)
            }
            names[ix] <- ni
        }

        off <- off + nc
    }

    names(y) <- names
    y <- as_dataset(y)
    keys(y) <- keys

    y
}


rbind.dataset <- function(..., check = TRUE, deparse.level = 1)
{
    # ignore 'deparse.level'
    check <- arg_option(check)

    x <- list(...)
    null <- vapply(x, is.null, NA)
    x <- lapply(x[!null], as_dataset)
    n <- length(x)

    if (n == 0L) {
        return(NULL)
    }

    x1 <- x[[1L]]
    y <- vector("list", length(x1))
    nc <- length(x1)
    names <- names(x1)
    names(y) <- names

    if (check) {
        diff <- vapply(x, function(xi) {
            nm <- names(xi)
            (length(xi) != nc || (!is.null(nm) && !identical(nm, names)))
        }, NA)
        j <- which(diff)

        if (length(j) > 0L) {
            stop(sprintf("arguments 1 and %.0f have different columns",
                         j[[1L]]))
        }
    }

    for (j in seq_len(nc)) {
        elt <- x1[[j]]
        rows <- lapply(x, `[[`, j)

        if (is.null(dim(elt))) {
            col <- do.call(c, rows)
        } else {
            col <- do.call(rbind, rows)
        }

        y[[j]] <- col
    }

    as_dataset(y)
}
