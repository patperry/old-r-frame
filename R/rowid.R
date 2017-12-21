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

rowid <- function(x, keys, default = NA_integer_, ...)
{
    UseMethod("rowid")
}


rowid.default <- function(x, keys, default = NA_integer_, ...)
{
    x <- keys(x)
    rowid(as_keyset(x), keys, default, ...)
}


rowid.NULL <- function(x, keys, default = NA_integer_, ...)
{
    if (is.null(keys)) {
        return(NULL)
    }
    keys <- as_dataset(keys)
    default <- arg_rowid_default(default)
    rep(default, nrow(keys))
}


rowid.keyset <- function(x, keys, default = NA_integer_, ...)
{
    x <- arg_keyset(x)
    if (!is.null(keys)) {
        keys <- as_dataset(keys)
    }
    default <- arg_rowid_default(default)

    if (is.null(keys)) {
        return(NULL)
    }

    n <- length(x)
    if (length(keys) != n) {
        stop("number of 'keys' components (%.0f) must 'x' components (%.0f)",
             length(keys), n)
    }

    for (i in seq_len(n)) {
        ki <- keys[[i]]

        if (length(dim(ki)) > 1L) {
            stop("cannot index with rank-%.0f array for key value",
                 length(dim(ki)))
        }

        xk <- x[[i]]

        ki <- if (is.integer(xk)) { as.integer(ki) }
        else if  (is.logical(xk)) { as.logical(ki) }
        else if  (is.complex(xk)) { as.complex(ki) }
        else if  (is.double(xk))  { as.double(ki) }
        else { as_utf8(as.character(ki)) }

        keys[[i]] <- ki
    }

    key_index(x, keys, default)
}


lookup <- function(keys, x, default = NA_integer_, ...)
{
    rowid(x, keys, default, ...)
}


arg_rowid_default <- function(value, call = sys.call(-1L))
{
    if (length(value) != 1L) {
        stop(simpleError("'default' must have length 1", call))
    }
    value <- trunc(as.double(value))
    if (is.nan(value) || is.infinite(value)) {
        stop(simpleError("'default' cannot be NaN or Inf", call))
    }
    if (is.na(value) || abs(value) <= .Machine$integer.max) {
        value <- as.integer(value)
    }
    value
}
