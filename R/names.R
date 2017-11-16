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
    nkey <- attr(x, "nkey")
    if (nkey == 0) {
        NULL
    } else {
        l <- as.list(x)
        rn <- lapply(l[1:nkey], function(xk) as_utf8(as.character(xk)))
        key_join(rn)
    }
}


`row.names<-.dataset` <- function(x, value)
{
    if (!is.null(value)) {
        stop("setting row names is not allowed for dataset objects")
    }
    x
}


names.dataset <- function(x)
{
    nm <- NextMethod()
    nkey <- attr(x, "nkey")
    if (nkey) {
        nm <- nm[-seq_len(nkey)]
    }
    nm
}


`names<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        stop("setting 'names' to NULL is not allowed")
    }
    with_rethrow({
        value <- as_names("'names'", value, length(x))
    })
    old <- attr(x, "names")
    nkey <- attr(x, "nkey")
    key <- old[seq_len(nkey)]

    dup <- intersect(key, value)
    if (length(dup) > 0) {
        stop(sprintf("duplicate column name: \"%s\" (matches key name)",
                     dup[[1]]))
    }

    attr(x, "names") <- c(key, value)
    x
}
