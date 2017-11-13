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

dimnames.dataset <- function(x)
{
    cn <- names(x)
    rn <- row.names(x)
    list(rn, cn)
}

`dimnames<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        stop("setting 'dimnames' to NULL is not allowed")
    } else if (length(value) != 2) {
        stop(sprintf("'dimnames' length (%d) must be 2", length(value)))
    } else if (!is.null(value[[1]])) {
        stop("setting row names is not allowed for dataset objects")
    }
    names(x) <- value[[2]]
    x
}

row.names.dataset <- function(x)
{
    k <- key(x)
    if (is.null(k)) {
        NULL
    } else {
        x[[k]]
    }
}

`row.names<-.dataset` <- function(x, value)
{
    if (!is.null(value)) {
        stop("setting row names is not allowed for dataset objects")
    }
    x
}


`names<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        stop("setting 'names' to NULL is not allowed")
    }
    with_rethrow({
        value <- as_names("'names'", value, length(x))
    })
    attr(x, "names") <- value
    x
}
