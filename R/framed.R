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

framed <- function(x, keys, ...)
{
    UseMethod("framed")
}


framed.default <- function(x, keys, ...)
{
    x <- as_dataset(x)
    framed(x, keys, ...)
}


framed.dataset <- function(x, keys, ...)
{
    if (is.null(keys)) {
       # pass
    } else if (length(dim(keys)) < 2L) {
        j <- arg_key_cols(x, keys)
        if (length(j) > 0) {
            keys <- as_keyset(x[j])
            x <- x[-j]
        } else {
            keys <- NULL
        }
    } else {
        keys <- as_keyset(keys)
    }

    keys(x) <- keys
    x
}
