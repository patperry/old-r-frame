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


as_simple_vector <- function(x, like = NULL, ...)
{
    UseMethod("as_simple_vector")
}


as_simple_vector.default <- function(x, like = NULL, ...)
{
    r <- length(dim(x))
    if (r > 1L) {
        stop(sprintf("cannot convert rank-%.0f array to simple vector", r))
    }

    if (is.numeric(x)) {
        if (is.integer(x)) {
            x <- as.integer(x)
        } else {
            x <- as.double(x)
        }
    } else if (inherits(x, "Date")) {
        x <- as_date(x)
    } else if (inherits(x, "POSIXt")) {
        x <- as_posixct(x)
    } else if (is.character(x) || !is.null(oldClass(x)) || isS4(x)) {
        x <- as.character(x)
        x <- as_utf8(x)
    } else if (is.logical(x)) {
        x <- as.logical(x)
    } else if (is.complex(x)) {
        x <- as.complex(x)
    } else {
        stop(sprintf("cannot convert objects of type \"%s\" to simple vector",
                     class(x)[[1L]]))
    }

    if (!is.null(like)) {
        na_old <- is.na(x)
        suppressWarnings(x <- cast_as(like, x))
        na_new <- is.na(x)

        failed <- which(na_new & !na_old)
        if (length(failed) > 0L) {
            attr(x, "failed") <- failed
        }
    }

    x
}


cast_as <- function(like, x)
{
    UseMethod("cast_as")
}


cast_as.logical <- function(like, x)
{
    as.logical(x)
}


cast_as.integer <- function(like, x)
{
    as.integer(x)
}


cast_as.numeric <- function(like, x)
{
    as.double(x)
}


cast_as.complex <- function(like, x)
{
    as.complex(x)
}


cast_as.character <- function(like, x)
{
    x <- as.character(x)
    x[!utf8_valid(x)] <- NA_character_
    as_utf8(x)
}


cast_as.Date <- function(like, x)
{
    as_date(x)
}


cast_as.POSIXct <- function(like, x)
{
    as_posixct(x, attr(like, "tzone"))
}


is_simple_vector <- function(x)
{
    if (!is.null(dim(x))) {
        return(FALSE)
    }

    cl <- oldClass(x)
    if (!is.null(cl)) {
        return(cl == "Date" || identical(cl, c("POSIXct", "POSIXt")))
    }

    if (is.character(x)) {
        return(all(Encoding(x) == "UTF-8" & utf8_valid(x)))
    }

    is.atomic(x) && !is.raw(x) && !is.null(x)
}
