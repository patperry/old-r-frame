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
    if (nc == 0) {
        return(0L)
    }

    nr <- vapply(x, nrow_column, 0) # not int since result might overflow
    i <- which.max(nr)
    n <- nr[[i]]
    j <- which(nr != 1 & nr != n)
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
    d <- dim(x)
    if (is.null(d)) {
        length(x)
    } else {
        d[[1]]
    }
}


downcast <- function(x, class)
{
    cl <- class(x)
    i <- match(class, cl)
    if (i > 1L) {
        cl <- cl[-seq_len(i - 1L)]
        class(x) <- cl
    }

    x
}


make_unique <- function(x)
{
    x <- as_dataset(x, simple = TRUE)
    if (!anyDuplicated(x)) {
        return(as_keyset(x))
    }
    keys <- unique(x)
    id <- lookup(x, keys)

    # TODO: implement in C?
    n <- nrow(x)
    copy <- integer(nrow(keys))
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
    as_keyset(x)
}
