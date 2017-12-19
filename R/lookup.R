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


rowid.keyset <- function(x, keys, default = NA_integer_, ...)
{
    if (!is_keyset(x)) {
        stop("argument is not a valid keyset object")
    }

    if (!is.null(keys)) {
        keys <- as_dataset(keys)
    }

    if (length(default) != 1L) {
        stop("'default' must have length 1")
    }
    default <- trunc(as.double(default))
    if (is.nan(default) || is.infinite(default)) {
        stop("'default' cannot be NaN or Inf")
    }
    if (is.na(default) || abs(default) <= .Machine$integer.max) {
        default <- as.integer(default)
    }

    key_index(x, keys, default)
}


lookup <- function(keys, x, default = NA_integer_, ...)
{
    rowid(x, keys, default, ...)
}
