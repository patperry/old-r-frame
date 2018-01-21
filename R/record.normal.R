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


## Normal Records

# A *normal record* is a record containing normalized values.
# Normalization places restrictions on the following types:
#
#   + `character`, all values are encoded in normalized UTF-8
#     and `NA` is not allowed (`""` is acceptable);
#
#   + `double`, the not-a-number value `NaN` is not allowed.
#
# Note that a normal record is not necessarily simple, and not all
# simple records are normal.

as.record.normal <- function(x)
    UseMethod("as.record.normal")

is.record.normal <- function(x)
{
    if (!is.record(x))
        FALSE
    else if (is.null(x) || inherits(x, "record.normal"))
        TRUE
    else
        type <- typeof(x)
        if (type == "character")
            isTRUE(all(x == utf8_normalize(x)))
        else if (type == "double")
            (!anyNA(x) || !any(is.nan(x)))
        else
            TRUE
}

# When coercing to a normal record, we replace invalid values with
# their normalized equivalents.

as.record.normal.default <- function(x)
{
    if (is.null(x) || inherits(x, "record.normal"))
        return(x)

    x <- as.record(x)

    type <- typeof(x)
    if (type == "character") {
        as.record.normal.character(x)
    } else if (type == "double")
        as.record.normal.double(x)
    else {
        class(x) <- c("record", "record.normal")
        x
    }
}

# For character, we convert values to normalized UTF-8, replace "" with NA.

as.record.normal.character <- function(x)
{
    x <- as.record.character(x)
    if (inherits(x, "record.normal"))
        return(x)

    if (!is.record.normal(x)) {
        x <- as.character(x)
        x <- utf8_normalize(x)
        x[!nzchar(x)] <- NA
        class(x) <- c("record", "record.normal")
    }

    x
}

# For double, we replace NaN with NA.

as.record.normal.double <- function(x)
{
    x <- as.record.double(x)
    if (inherits(x, "record.normal"))
        return(x)

    if (!is.record.normal(x)) {
        x[is.nan(x)] <- NA
        class(x) <- c("record", "record.normal")
        
    }

    x
}
