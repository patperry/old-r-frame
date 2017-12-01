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

as.list.dataset <- function(x, ..., flatten = FALSE)
{
    with_rethrow({
        flatten <- as_option("recursive", flatten)
    })

    x <- unclass(x)
    attr(x, "keys") <- NULL
    attr(x, "row.names") <- NULL

    if (flatten) {
        names <- as.list(names(x))
        names(x) <- NULL

        # convert each element to a list of columns, and compute the names
        for (i in seq_along(x)) {
            xi <- x[[i]]
            if (length(dim(xi)) <= 1) {
                x[[i]] <- list(xi)
                names[[i]] <- list(names[[i]])
            } else if (is.data.frame(xi)) {
                x[[i]] <- as.list(xi, ..., flatten = TRUE)
                names[[i]] <- paste(names[[i]], names(x[[i]]), sep = ".")
            } else {
                nm <- colnames(xi)
                if (is.null(nm)) {
                    nm <- as.character(seq_len(ncol(xi)))
                }
                x[[i]] <- lapply(seq_len(ncol(xi)),
                             function(j) xi[, j, drop = TRUE])
                names[[i]] <- paste(names[[i]], nm, sep = ".")
            }
        }

        x <- do.call(c, x)
        names(x) <- as.character(do.call(c, names))
    }
    x
}
