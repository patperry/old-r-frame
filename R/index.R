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

`[.dataset` <- function(x, ..., drop = FALSE)
{
    if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }

    cl <- match.call()
    stopifnot(length(cl) >= 2)
    if ("drop" %in% names(cl)) {
        cl[["drop"]] <- NULL
    }

    index <- vector("list", length(cl) - 2)
    for (k in seq_along(index)) {
        if (!(is.name(cl[[2 + k]]) && as.character(cl[[2 + k]]) == "")) {
            index[[k]] <- eval.parent(cl[[2 + k]])
        }
    }

    if (length(index) == 2 && !is.null(index[[1]])) {
        i <- index[[1]]
        if (!is.numeric(i)) {
            i <- match(as.character(i), rownames(x))
            return(x[i,,drop = drop])
        }
        if (!is.null(attr(x, "nkey"))) {
            dup <- anyDuplicated(i)
            if (dup > 0) {
                 stop(sprintf(
                    "subset contains duplicate row (%.0f) but key is non-NULL",
                    i[[dup]]))
            }
        }
    }

    y <- NextMethod()
    attr(y, "row.names") <- .set_row_names(nrow(y))
    y
}
