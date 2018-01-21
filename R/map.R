#  Copyright 2018 Patrick O. Perry.
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

do <- function(f, x, env = NULL)
{
    f <- as.function(f)
    x <- as.list(x)
    env <- if (is.null(env))
        parent.frame()
    else as.environment(env)

    do.call(f, x, FALSE, env)
}


map_.default <- function(x, f, ...)
{
    for (i in seq_along(x))
        f(x[[i]], ...)

    invisible(NULL)
}


map <- function(x, f, ...)
    UseMethod("map")

map.default <- function(x, f, ...)
    list.map(x, f, ...)


list.map <- function(x, f, ...)
    UseMethod("list.map")

list.map.default <- function(x, f, ...)
{
    n <- length(x)
    y <- vector("list", n)
    for (i in seq_len(n)) {
        y[[i]] <- f(x[[i]], ...)
    }
    as.record.list(lapply(x, f, ...))
}


NULL.map <- function(x, f, ...)
    UseMethod("logical.map")

NULL.map.default <- function(x, f, ...)
{
    for (i in seq_along(x)) {
        val <- f(x[[i]], ...)
        if (!is.null(val) && length(val) > 0) {
            fmt <- "`f` had non-NULL result for input `x[[%.0f]]`"
            stop(sprintf(fmt, i))
        }
    }

    NULL
}


logical.map <- function(x, f, ...)
    UseMethod("logical.map")

logical.map.default <- function(x, f, ...)
    as.record.logical(list.map(x, f, ...))


raw.map <- function(x, f, ...)
    UseMethod("raw.map")

raw.map.default <- function(x, f, ...)
    as.record.raw(list.map(x, f, ...))


integer.map <- function(x, f, ...)
    UseMethod("integer.map")

integer.map <- function(x, f, ...)
    as.record.integer(list.map(x, f, ...))


double.map <- function(x, f, ...)
    UseMethod("double.map")

double.map.default <- function(x, f, ...)
    as.record.double(list.map(x, f, ...))


complex.map <- function(x, f, ...)
    UseMethod("complex.map")

complex.map.default <- function(x, f, ...)
    as.record.complex(list.map(x, f, ...))


character.map <- function(x, f, ...)
    UseMethod("character.map")

character.map.default <- function(x, f, ...)
    as.record.character(list.map(x, f, ...))




numeric.map <- function(x, f, ...)
    UseMethod("numeric.map")

numeric.map.default <- function(x, f, ...)
{
    n <- length(x)
    y <- integer(n)
    last <- n

    for (i in seq_len(x)) {
        val <- f(x[[i]], ...)
        if (is.object(val) || !is.integer(val)) {
            last <- i - 1
            break
        }
        y[[i]] <- val
    }

    if (last == n)
        return(y)

    y <- as.double(y)
    first <- TRUE

    while (i <= n) {
        if (!first) {
            val <- f(x[[i]], ...)
            first <- FALSE
        }

        if (!is.numeric(val)) {
            fmt <- "`f` had non-numeric result for input `x[[%.0f]]`"
            stop(sprintf(fmt, i))
        }

        y[[i]] <- as.double(val)
        i <- i + 1
    }

    y
}
