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

keyset <- function(...)
{
    cl <- sys.call()
    cl[[1L]] <- quote(frame::dataset)
    x <- eval(cl, parent.frame())
    as_keyset(x)
}


as_keyset <- function(x, ...)
{
    UseMethod("as_keyset")
}


as_keyset.keyset <- function(x, ...)
{
    if (!is_keyset(x)) {
        stop("argument is not a valid keyset object")
    }
    cl <- class(x)
    i <- match("keyset", cl)
    if (i > 1L) {
        cl <- cl[-(1L:(i - 1L))]
        class(x) <- cl
    }

    x
}


as_keyset.default <- function(x, ...)
{
    x <- as_dataset(x)
    as_keyset(x, ...)
}


as_keyset.dataset <- function(x, ...)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset object")
    }

    with_rethrow({
        x <- as_atomset("key set", x)
    })

    if (length(x) == 1L) {
        i <- which(duplicated(x[[1L]]))
        if (length(i) > 0L) {
            j <- which(x[[1L]] == x[[1L]][[i[[1]]]])
            stopifnot(length(j) > 1L)
            stop(sprintf("key set has duplicate entries (%.0f and %.0f)",
                         j[[1L]], j[[2L]]))
        }
    } else {
        kv <- key_encode(x)
        i <- which(duplicated(kv))
        if (length(i) > 0L) {
            j <- which(kv == kv[[i[[1L]]]])
            stopifnot(length(j) > 1L)
            stop(sprintf("key set has duplicate rows (%.0f and %.0f)",
                         j[[1L]], j[[2L]]))
        }
    }

    class(x) <- c("keyset", class(x))
    x
}


is_keyset <- function(x)
{
    is_dataset(x) && inherits(x, "keyset")
}
