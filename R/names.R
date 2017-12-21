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
        row.names(x) <- NULL
        names(x) <- NULL
    } else if (length(value) != 2) {
        stop(sprintf("'dimnames' length (%.0f) must be 2", length(value)))
    } else {
        row.names(x) <- value[[1L]]
        names(x) <- value[[2L]]
    }
    x
}


row.names.dataset <- function(x)
{
    keys <- keys(x)
    if (is.null(keys)) {
        NULL
    } else if (length(keys) == 1 && is.character(keys[[1L]])) {
        keys[[1L]]
    } else {
        NULL
    }
}


`row.names<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        keys(x) <- NULL
    } else {
        value <- arg_names(nrow(x), "rows", as.character(value),
                           allow_na = FALSE, unique = TRUE)
        keys(x) <- dataset(name = value)
    }
    x
}
