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

slice <- function(x, ...)
{
    UseMethod("slice")
}


slice.default <- function(x, ...)
{
    x <- keys(x)
    slice(as_keyset(x), ...)
}


slice.keyset <- function(x, ...)
{
    i <- arg_slice(x, list(...))

    n <- nrow(x)
    ni <- length(i)
    mask <- rep(TRUE, n)

    for (k in seq_len(ni)) {
        ik <- i[[k]]
        if (is.null(ik)) {
            next
        }
        xk <- x[[k]]
        mask <- mask & (xk %in% ik)
    }

    which(mask)
}
