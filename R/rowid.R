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


lookup <- function(keys, x, default = NA_integer_, ...)
{
    rowid(x, keys, default, ...)
}


rowid <- function(x, keys, default = NA_integer_, ...)
{
    UseMethod("rowid")
}


rowid.default <- function(x, keys, default = NA_integer_, ...)
{
    x <- keys(x)
    rowid(x, keys, default, ...)
}


rowid.NULL <- function(x, keys, default = NA_integer_, ...)
{
    if (is.null(keys)) {
        return(NULL)
    }
    keys <- as_dataset(keys)
    default <- arg_rowid_default(default)
    rep(default, nrow(keys))
}


rowid.keyset <- function(x, keys, default = NA_integer_, ...)
{
    x <- arg_keyset(x)
    if (!is.null(keys)) {
        keys <- as_dataset(keys)
    }
    default <- arg_rowid_default(default)

    if (is.null(keys)) {
        return(NULL)
    }

    n <- length(x)
    if (length(keys) != n) {
        stop("number of 'keys' components (%.0f) must 'x' components (%.0f)",
             length(keys), n)
    }

    failed <- NULL

    for (i in seq_len(n)) {
        ki <- keys[[i]]

        if (length(dim(ki)) > 1L) {
            stop("cannot index with rank-%.0f array for key value",
                 length(dim(ki)))
        }

        ki <- as_simple_vector(ki, x[[i]])
        failed <- c(failed, attr(ki, "failed"))

        keys[[i]] <- ki
    }

    row <- key_index(x, keys, default)
    row[failed] <- NA_integer_
    row
}


# this is just a temporary implementation. it works, but it's really slow,
# especially for double and complex
key_index <- function(x, i, default = NA_integer_)
{
    if (length(x) == 0L) {
        return(rep(1L, nrow(i)))
    }

    id <- seq_len(nrow(x))
    names(id) <- key_encode(x)

    id <- id[key_encode(i)]
    id[is.na(id)] <- default

    unname(id)
}


key_encode <- function(x)
{
    nk <- length(x)
    if (nk == 0) {
        NULL
    } else if (nk == 1) {
        key_escape(x[[1]])
    } else {
        do.call(paste, c(unname(lapply(x, key_escape)), sep = ","))
    }
}


key_escape <- function(x)
{
    # encode as character
    if (is.double(x) || is.complex(x)) {
        x <- format(x, digits = 22, trim = TRUE)
    } else if (is.character(x)) {
        # replace '\' with '\\', '"' with '\"',
        x[!is.na(x)] <- gsub("(\\\\|\")", "\\\\\\1", x[!is.na(x)])
        x <- ifelse(is.na(x), "NA", paste0('"', x, '"'))
    } else {
        x <- as.character(x)
        x[is.na(x)] <- "NA"
    }
    x
}


arg_rowid_default <- function(value, call = sys.call(-1L))
{
    if (length(value) != 1L) {
        stop(simpleError("'default' must have length 1", call))
    }
    value <- trunc(as.double(value))
    if (is.nan(value) || is.infinite(value)) {
        stop(simpleError("'default' cannot be NaN or Inf", call))
    }
    if (is.na(value) || abs(value) <= .Machine$integer.max) {
        value <- as.integer(value)
    }
    value
}
