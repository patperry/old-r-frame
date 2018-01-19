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


record <- function(...)
{
    args <- as.list(substitute(list(...)))[-1L]
    x <- list(...)
    n <- length(x)

    names <- names(x)
    if (is.null(names)) {
        names <- character(n)
    }
    for (i in seq_len(n)) {
        if (names[[i]] == "") {
            names[[i]] <- deparse(args[[i]], nlines = 1L)[1L]
        }
    }
    names(x) <- names
    as_record(x)    
}


as_record <- function(x, names = NULL, ...)
{
    UseMethod("as_record")
}


as_record.record <- function(x, names = NULL, ...)
{
    if (!is_record(x)) {
        x <- as_record(x)
    }
    if (!is.null(names)) {
        names(x) <- names
    }
    x
}


as_record.default <- function(x, names = NULL, ...)
{
    x <- as.list(x)
    as_record(x, names)
}


as_record.list <- function(x, names = NULL, ...)
{
    if (is.null(names)) {
        names <- names(x)
    }
    x <- structure(as.vector(x), class = "record")
    names(x) <- names
    x
}


is_record <- function(x)
{
    inherits(x, "record")
}


`names<-.record` <- function(x, value)
{
    n <- length(x)
    names <- arg_names(n, "fields", value, allow_na = TRUE, utf8 = TRUE)
    attr(x, "names") <- names
    x
}
