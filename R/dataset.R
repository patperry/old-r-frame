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

dataset <- function(..., frame = NULL)
{
    qs <- quos(..., .named = TRUE)
    names <- names(qs)

    x <- list()
    for (i in seq_along(qs)) {
        nm <- names[[i]]
        y <- eval_tidy(qs[[i]], data = x)

        # safer than 'x[[nm]] <- y' since y might be NULL or nm duplicated
        x <- c(x, list(y))
        names(x)[[i]] <- nm
    }

    as_dataset(x, frame = frame)
}


as_dataset <- function(x, ...)
{
    UseMethod("as_dataset")
}


as_dataset.default <- function(x, ...)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    as_dataset(x, ...)
}


as_dataset.data.frame <- function(x, ..., rownames = "name")
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    # fix column names if they are missing
    if (is.null(names(x))) {
        names(x) <- paste0("V", seq_along(x))
    }

    # if row names are present, optionally convert them to a column
    if (.row_names_info(x) > 0 && !is.null(rownames)) {
        if (rownames %in% names(x)) {
            stop(sprintf("cannot create '%s' column for row names; already exists", rownames))
        }
        xx <- c(list(rownames(x)), as.list(x))
        names(xx) <- c(rownames, names(x))
        x <- structure(xx, class = "data.frame",
                       row.names = .set_row_names(nrow(x)))
    } else {
        rownames(x) <- NULL
    }

    class(x) <- c("dataset", "data.frame")
    x
}


as_dataset.list <- function(x, ...)
{
    if (!is.list(x)) {
        stop("argument is not a list")
    }

    names <- names(x)

    # fix column names if they are missing
    if (is.null(names)) {
        names(x) <- names <- sprintf("V%d", seq_along(x))
    }

    if ((i <- anyDuplicated(names))) {
        stop(sprintf("duplicate column name: '%s'", names[[i]]))
    }

    nc <- length(x)
    if (nc == 0) {
        nr <- 0L
    } else {
        nr <- col_nrow(x[[1]])
        for (i in seq.int(2, length.out = nc - 1)) {
            elt <- x[[i]]
            n <- col_nrow(elt)
            if (n != nr) {
                stop(sprintf("columns %d and %d have differing numbers of rows: %d and %d",
                             1, i, nr, n))
            }
        }
    }

    structure(x, class = c("dataset", "data.frame"),
              row.names = .set_row_names(nr))
}


col_nrow <- function(x)
{
    d <- dim(x)
    if (is.null(d)) {
        length(x)
    } else {
        d[[1]]
    }
}


as_dataset.dataset <- function(x, ...)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }
    class(x) <- c("dataset", "data.frame")
    x
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}
