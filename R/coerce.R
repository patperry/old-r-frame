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


as.data.frame.dataset <- function(x, row.names = NULL, optional = FALSE, ...,
                                  stringsAsFactors = FALSE)
{
    x <- as_dataset(x)
    if (missing(row.names)) {
        row.names <- row.names(x)
    } else if (!is.null(row.names)) {
        row.names <- arg_names(nrow(x), "rows", row.names,
                               allow_na = FALSE, unique = TRUE)
    }
    optional <- arg_option(optional)
    stringsAsFactors <- arg_option(stringsAsFactors)

    if (length(x) == 0) {
        n <- dim(x)[[1]]
        df <- structure(list(), row.names = .set_row_names(n),
                        class = "data.frame")
        row.names(df) <- row.names(x)
        return(as.data.frame(df, row.names = row.names))
    }
    as.data.frame(as.list(x, flat = TRUE), row.names = row.names,
                  optional = optional, ..., stringsAsFactors = stringsAsFactors)
}


as.list.dataset <- function(x, ..., flat = FALSE, path = FALSE)
{
    x <- as_dataset(x)
    flat <- arg_option(flat)
    path <- arg_option(path)

    x <- unclass(x)
    attr(x, "keys") <- NULL
    attr(x, "row.names") <- NULL

    if (flat) {
        names <- onames <- names(x)
        if (is.null(names)) {
            names <- as.character(seq_along(x))
        }
        names <- as.list(names)
        names(x) <- NULL
        index <- vector("list", length(x))
        path_ <- vector("list", length(x))

        # convert each element to a list of columns, and compute the names
        for (i in seq_along(x)) {
            xi <- x[[i]]
            d <- dim(xi)
            n <- d[[2L]]
            if (length(d) <= 1) {
                x[[i]] <- list(xi)
                index[[i]] <- list(i)
                path_[[i]] <- list(names[[i]])
                names[[i]] <- list(names[[i]])
            } else if (n == 0L) {
                nrow <- d[[1L]]
                if (nrow > 0L) {
                    if (is.data.frame(xi)) {
                        elt <- as.list(xi[1L, , drop = FALSE])
                    } else {
                        elt <- xi[1L, , drop = TRUE]
                    }
                    x[[i]] <- list(rep(list(elt), nrow))
                } else {
                    x[[i]] <- list()
                }
                index[[i]] <- list(i)
                path_[[i]] <- list(names[[i]])
                names[[i]] <- list(names[[i]])
            } else {
                if (is.data.frame(xi)) {
                    x[[i]] <- as.list(xi, ..., flat = TRUE, path = TRUE)
                } else {
                    x[[i]] <- lapply(seq_len(n),
                                     function(j) xi[, j, drop = TRUE])
                }

                nm <- names(x[[i]])
                if (is.null(nm)) {
                    nm <- as.character(seq_len(n))
                }
                names(x[[i]]) <- nm

                ii <- attr(x[[i]], "index")
                if (is.null(ii)) {
                    ii <- seq_len(n)
                }
                index[[i]] <- lapply(ii, function(iij) c(i, iij))

                pi_ <- attr(x[[i]], "path")
                if (is.null(pi_)) {
                    pi_ <- nm
                }

                path_[[i]] <- lapply(pi_, function(pij) c(names[[i]], pij))
                names[[i]] <- paste(names[[i]], nm, sep = ".")
            }
        }

        x <- do.call(c, x)
        if (is.null(x)) {
            x <- list()
        }
        if (!is.null(onames)) {
            names(x) <- as.character(do.call(c, names))
        }
        if (path) {
            attr(x, "index") <- do.call(c, index)
            attr(x, "path") <- do.call(c, path_)
        }
    } else if (path) {
        attr(x, "index") <- as.list(seq_along(x))
        attr(x, "path") <- as.list(names(x))
    }
    x
}


as.matrix.dataset <- function(x, rownames.force = NA, ...)
{
    x <- as_dataset(x)
    df <- as.data.frame(x, optional = TRUE)
    as.matrix(df, rownames.force, ...)
}
