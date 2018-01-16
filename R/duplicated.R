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

    fromLast <- arg_option(fromLast)

    if (length(x) == 0) {
        n <- nrow(x)
        if (n == 0) {
            return(logical())
        } else if (fromLast) {
            return(c(rep(TRUE, n - 1L), FALSE))
        } else {
            return(c(FALSE, c(rep(TRUE, n - 1L))))
        }
    }

    x <- as_dataset(x, simple = TRUE)
    duplicated(key_encode(x), fromLast = fromLast)
}


duplicated.keyset <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    logical(nrow(x))
}


unique.dataset <- function(x, incomparables = FALSE, ..., sorted = FALSE)
{
    if (!identical(incomparables, FALSE)) {
        warning("'incomparables' argument is ignored")
    }

    if (length(x) == 0) {
        if (nrow(x) == 0) {
            return(as_keyset(NULL))
        } else {
            return(as_keyset(structure(list(), row.names = .set_row_names(1),
                                       class = "data.frame")))
        }
    }

    dup <- duplicated(x)
    keys <- x[!dup, ]
    keys <- as_keyset(keys)

    if (sorted) {
        unique.keyset(keys, sorted = TRUE)
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
