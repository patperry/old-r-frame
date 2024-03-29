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


arg_record_names <- function(n, value, name = argname(substitute(value)),
                             call = sys.call(-1))
{
    if (missing(value) || is.null(value))
        return(NULL)

    raw <- as.character(value)
    if (anyNA(raw))
        raw[is.na(raw)] <- ""

    names <- tryCatch(as_utf8(raw, normalize = TRUE), error = function(cond) NULL)
    if (is.null(names)) {
        invalid <- which(!utf8_valid(raw))[[1]]
        fmt <- "%s entry %.0f has wrong character encoding"
        stop(simpleError(sprintf(fmt, name, invalid), call))
    }

    if (n >= 0) {
        nvalue <- length(names)
        if (nvalue != n) {
            fmt <- "mismatch: %s length is %.0f, object length is %.0f"
            stop(simpleError(sprintf(fmt, name, nvalue, n), call))
        }
    }

    names
}


arg_record_subset <- function(x, value, call = sys.call(-1))
{
    if (missing(value) || is.null(value)) {
        NULL
    } else if (is.numeric(value)) {
        as.numeric(value)
    } else if (is.logical(value)) {
        i <- as.logical(value)
        n <- length(x)
        ni <- length(i)
        if (ni != n) {
            fmt <- "mismatch: logical index length is %.0f, object length is %.0f"
            stop(simpleError(sprintf(fmt, ni, n), call))
        }
        i
    } else {
        match(as.character(value), names(x))
    }
}


arg_record_index1 <- function(n, i, call = sys.call(-1L))
{
    if (n == 0)
        stop("missing index")

    i1 <- i[[1]]

    n1 <- length(i1)
    if (n1 != 1)
        stop(sprintf("non-scalar index (length %.0f)", n1))

    i1
}
