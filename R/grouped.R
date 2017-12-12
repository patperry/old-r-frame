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

grouped <- function(x, by = NULL, do = NULL, ...)
{
    xlab <- NULL
    if (is.null(do)) {
        do <- function(x) x
        xlab <- "data"
    } else if (!is.function(do)) {
        stop("'do' argument should be a function (or NULL)")
    }

    x <- framed(x)
    args <- c(list(NULL), list(...))

    if (is.null(by)) {
        args[[1L]] <- x
        y <- list(do.call(do, args))
        ans <- framed(list(y))
    } else {
        by <- framed(by)
        g <- key_encode(by)
        y <- split(x, g)
        nm <- names(y)
        k <- key_decode(nm, composite = length(by) > 1L)
        for (i in seq_along(k)) {
            storage.mode(k[[i]]) <- storage.mode(by[[i]])
        }
        if (is.null(names(by)) && length(k) == 1L) {
            names(k) <- "group"
        } else {
            names(k) <- names(by)
        }
        ans <- framed(list(y), framed(k))
    }

    if (!is.null(xlab)) {
        names(ans) <- xlab
    }
    ans
}
