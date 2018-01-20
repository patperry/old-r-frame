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

schema <- function(x, ...)
{
    UseMethod("schema")
}


schema.default <- function(x, ...)
{
    n <- length(x)
    type <- if (n == 0)
        NULL
    else if (n == 1 && is.null(dim(x)))
        x[0]
    else x
    as.record(list(type))
}


schema.record <- function(x, ...)
{
    x <- as.record(x)
    as.record(lapply(x, schema))
}


schema.dataset <- function(x, ...)
{
    # TODO: fix
    id <- seq_along(x)
    name <- names(x)
    class <- vapply(x, function(elt) class(elt)[[1L]], "")

    if (is.null(name)) {
        s <- dataset(class)
    } else {
        s <- dataset(name, class)
    }

    keys(s) <- keyset(id)

    s
}
