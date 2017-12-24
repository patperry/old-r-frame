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

anyDuplicated.dataset <- function(x, incomparables = FALSE, fromLast = FALSE,
                                  ...)
{
    dup <- duplicated(x, incomparables = incomparables,
                      fromLast = fromLast, ...)
    j <- which(dup)
    if (length(j) > 0L) j[[1L]] else 0L
}


anyDuplicated.keyset <- function(x, incomparables = FALSE, fromLast = FALSE,
                                 ...)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    FALSE
}


duplicated.dataset <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    duplicated(key_encode(x), fromLast = fromLast)
}


duplicated.keyset <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    logical(nrow(x))
}


transform.dataset <- function(`_data`, ..., `_enclos` = parent.frame())
{
    x <- `_data`
    exprs <- substitute(list(...))
    enclos <- `_enclos`

    if (!is.environment(enclos) && !is.null(enclos)) {
        stop("'enclos' must be an environment or NULL")
    }

    y <- eval(exprs, x, enclos)
    names <- names(y)
    if (!is.null(names)) {
        for (i in seq_along(y)) {
            nm <- names[[i]]
            if (nm != "") {
                x[[nm]] <- y[[i]]
            }
        }
    }

    x
}


unique.dataset <- function(x, incomparables = FALSE, ..., sorted = FALSE)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    if (length(x) == 0) {
        return(as_keyset(NULL))
    }

    keys <- x[!duplicated(x), ]
    keys <- as_keyset(keys)

    if (sorted) {
        unique.keyset(keys, ..., sorted = TRUE)
    } else {
        keys
    }
}


unique.keyset <- function(x, incomparables = FALSE, ..., sorted = FALSE)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    sorted <- arg_option(sorted)
    
    if (sorted) {
        # use radix sort for consistent, fast sorting, regardless of locale;
        # requires R >= 3.3
        cols <- unname(as.list(x))
        o <- do.call(order, c(cols, method = "radix"))
        x <- x[o,]
    }

    as_keyset(x)
}
