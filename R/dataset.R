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

dataset <- function(...)
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
            name <- deparse(args[[i]], nlines = 1L)[1L]
            names[[i]] <- sub("^I\\((.*)\\)$", "\\1", name)
        }
    }
    names(x) <- names
    framed(x)
}


is_dataset <- function(x)
{
    is.data.frame(x) && inherits(x, "dataset")
}
