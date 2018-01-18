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

keyset <- function(...)
{
    cl <- sys.call()
    cl[[1L]] <- quote(frame::dataset)
    x <- eval(cl, parent.frame())
    as_keyset(x)
}


as_keyset <- function(x, ...)
{
    UseMethod("as_keyset")
}


as_keyset.keyset <- function(x, ...)
{
    if (!is_keyset(x)) {
        x <- as_keyset(x)
    }
    x
}


as_keyset.default <- function(x, ...)
{
    x <- as_dataset(x)
    as_keyset(x, ...)
}


as_keyset.dataset <- function(x, ...)
{
    x <- as_dataset(x, simple = TRUE)
    keys(x) <- NULL

    if ((j <- anyDuplicated(x))) {
        stop(sprintf("argument has a duplicate row (%.0f)", j))
    }

    class(x) <- c("keyset", class(x))
    x
}


is_keyset <- function(x)
{
    is_dataset(x) && inherits(x, "keyset")
}
