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

dataset <- function(...)
{
    x <- data.frame(..., row.names = NULL, check.rows = FALSE,
                    check.names = TRUE, fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)
    as_dataset(x)
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
    class(x) <- "data.frame"

    # fix column names if they are missing
    if (is.null(names(x))) {
        names(x) <- paste0("V", seq_along(x))
    }

    # if row names are present, optionally convert them to a column
    if (.row_names_info(x) > 0 && !is.null(rownames)) {
        if (rownames %in% names(x)) {
            stop(sprintf(
                "cannot create '%s' column for row names; already exists",
                rownames))
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