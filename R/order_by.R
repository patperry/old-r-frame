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

order_by <- function(x, ..., enclos = parent.frame())
{
    UseMethod("order_by")
}


order_by.default <- function(x, ..., enclos = parent.frame())
{
    x <- as_dataset(x)
    order_by(x, ..., enclos = enclos)
}


order_by.dataset <- function(x, ..., enclos = parent.frame())
{
    exprs <- substitute(list(...))

    if (!is.environment(enclos) && !is.null(enclos)) {
        stop("'enclos' must be an environment or NULL")
    }

    i <- eval(exprs, x, enclos)
    if (length(i) == 0L) {
        return(x)
    }

    names <- names(i)
    if (!is.null(names)) {
        if (!all(names %in% c("asc", "desc", ""))) {
            stop("named arguments must be 'asc' or 'desc'")
        }
        desc <- (names == "desc")
        names(i) <- NULL
    } else {
        desc <- FALSE
    }

    i[["decreasing"]] <- desc
    i[["method"]] <- "radix"
    o <- do.call(order, i)

    x[o,]
}
