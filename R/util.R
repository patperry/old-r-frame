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


as_column <- function(x, n)
{
    # promote scalars
    if (length(x) == 1 && length(dim(x)) <= 1) {
        x <- rep(x, n)
    }

    # drop keys
    if (!is.null(keys(x))) {
        keys(x) <- NULL
    }

    # drop names
    d <- dim(x)
    if (is.null(d)) {
        names(x) <- NULL
    } else if (length(d) == 1) {
        dimnames(x) <- NULL
    } else {
        rownames(x) <- NULL
    }

    x
}


nrow_dataset <- function(x)
{
    nc <- length(x)
    if (nc == 0L) {
        return(0L)
    }

    nr <- vapply(x, nrow_column, 0)
    i <- which.max(nr)
    n <- nr[[i]]
    if (n == -1L) { # all columns are NULL
        return(0L)
    }
    j <- which(!nr %in% c(-1L, 1L, n))
    if (length(j) > 0) {
        names <- names(x)
        stop(sprintf(
            "columns %d and %d (\"%s\" and \"%s\") have differing numbers of rows: %.0f and %.0f",
            i, j[[1]], names[[i]], names[[j[[1]]]],
            n, nr[[j[[1]]]]))
    }

    n
}


nrow_column <- function(x)
{
    if (is.null(x)) {
        -1L
    } else {
        d <- dim(x)
        if (is.null(d)) {
            length(x)
        } else {
            d[[1]]
        }
    }
}


make_unique <- function(x)
{
    x <- as_dataset(x, simple = TRUE)
    if (!anyDuplicated(x)) {
        return(as_keyset(x))
    }

    keys <- unique(x)
    nkey <- nrow(keys)
    id <- lookup(x, keys)

    y <- append_copy_num(x, nkey, id)
    as_keyset(y)
}


append_copy_num <- function(x, nkey, id)
{
    # TODO: implement in C?
    copy <- integer(nkey)
    newkey <- integer(length(id))
    for (i in seq_along(id)) {
        k <- id[[i]]
        copy[[k]] <- copy[[k]] + 1L
        newkey[[i]] <- copy[[k]]
    }
    names <- names(x)
    if (is.null(names)) {
        names <- character(length(x))
    }

    x[[length(x) + 1L]] <- newkey
    names(x) <- c(names, "#")

    x
}
