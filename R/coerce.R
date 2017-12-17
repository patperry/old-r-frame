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

### Basic Types ###

as_character_scalar <- function(name, value, utf8 = TRUE)
{
    if (is.null(value)) {
        return(NULL)
    }
    value <- as_character_vector(name, value, utf8)
    if (length(value) != 1) {
        stop(sprintf("'%s' must be a scalar character string", name))
    }
    value
}


as_character_vector <- function(name, value, utf8 = TRUE)
{
    if (!(is.null(value) || is.character(value) || is.factor(value)
          || all(is.na(value)))) {
        stop(sprintf("'%s' must be a character vector or NULL", name))
    }
    if (is.null(value)) {
        return(NULL)
    }
    value <- as.character(value)
    if (utf8) {
        value <- as_utf8(value)
    }
    value
}


as_double_scalar <- function(name, value, allow_null = FALSE)
{
    if (is.null(value)) {
        if (allow_null) {
            return(NULL)
        } else {
            stop(sprintf("'%s' cannot be NULL", name))
        }
    }

    if (length(value) != 1) {
        stop(sprintf("'%s' must have length 1", name))
    }

    if (!(is.numeric(value) && !is.nan(value) && !is.na(value))) {
        stop(sprintf("'%s' must be a numeric value%s", name,
                     if (allow_null) " (or NULL)" else ""))
    }

    as.double(value)
}


as_enum <- function(name, value, choices)
{
    if (!(is.character(value) && length(value) == 1 && !is.na(value))) {
        stop(sprintf("'%s' must be a character string", name))
    }

    i <- pmatch(value, choices, nomatch = 0)
    if (all(i == 0)) {
        stop(sprintf("'%s' must be one of the following: ", name),
             paste(dQuote(choices), collapse = ", "))
    }
    i <- i[i > 0]
    choices[[i]]
}


as_integer_scalar <- function(name, value, nonnegative = FALSE)
{
    if (is.null(value)) {
        return(NULL)
    }
    value <- as_integer_vector(name, value, nonnegative)
    if (length(value) != 1) {
        stop(sprintf("'%s' must have length 1", name))
    }
    value
}


as_integer_vector <- function(name, value, nonnegative = FALSE)
{
    if (is.null(value)) {
        return(NULL)
    }

    if (!(is.numeric(value) || all(is.na(value)))) {
        stop(sprintf("'%s' must be integer-valued", name))
    }

    value <- as.integer(value)
    if (nonnegative && any(!is.na(value) & value < 0)) {
        stop(sprintf("'%s' must be non-negative", name))
    }

    value
}


as_nonnegative <- function(name, value)
{
    if (is.null(value)) {
        return(NULL)
    }
    value <- as_integer_scalar(name, value, nonnegative = TRUE)
    if (is.na(value)) {
        stop(sprintf("'%s' cannot be NA", name))
    }
    value
}


as_option <- function(name, value)
{
    if (is.null(value)) {
        return(FALSE)
    }

    if (!(length(value) == 1 && is.logical(value) && !is.na(value))) {
        stop(sprintf("'%s' must be TRUE or FALSE", name))
    }
    as.logical(value)
}

### Dataset ###


as_column <- function(x, n)
{
    # promote scalars
    if (length(x) == 1 && length(dim(x)) <= 1) {
        x <- rep(x, n)
    }

    # drop keys
    if (!is.null(keys(x))) {
        keys(x) <- NULL
    }

    # drop names
    d <- dim(x)
    if (is.null(d)) {
        names(x) <- NULL
    } else if (length(d) == 1) {
        dimnames(x) <- NULL
    } else {
        rownames(x) <- NULL
    }

    x
}


nrow_dataset <- function(x)
{
    nc <- length(x)
    if (nc == 0) {
        return(0L)
    }

    nr <- vapply(x, nrow_column, 0) # not int since result might overflow
    i <- which.max(nr)
    n <- nr[[i]]
    j <- which(nr != 1 & nr != n)
    if (length(j) > 0) {
        names <- names(x)
        stop(sprintf(
            "columns %d and %d (\"%s\" and \"%s\") have differing numbers of rows: %.0f and %.0f",
            i, j[[1]], names[[i]], names[[j[[1]]]],
            n, nr[[j[[1]]]]))
    }

    n
}


nrow_column <- function(x)
{
    d <- dim(x)
    if (is.null(d)) {
        length(x)
    } else {
        d[[1]]
    }
}


as_names <- function(name, value, n)
{
    # NULL names allowed
    if (is.null(value)) {
        return(NULL)
    }

    if (length(value) != n) {
        stop(sprintf("%s length (%.0f) must match number of columns (%.0f)",
                     name, length(value), n))
    }

    # fix missing names
    value[is.na(value)] <- ""

    value
}


as_by <- function(name, value, x)
{
    if (is.null(value) || length(value) == 0L) {
        return(NULL)
    }

    n <- if (is.null(x)) 0L else dim(x)[[1L]]

    if (length(dim(value)) < 2L) {
        j <- as_by_cols(name, value, x)
        value <- framed(x[, j, drop = FALSE])
    } else {
        value <- framed(value)
        nv <- dim(value)[[1L]]
        if (nv != n) {
            stop(sprintf("'%s' rows (%.0f) must match data rows (%.0f)",
                         name, nv, n))
        }
    }

    keys(value) <- NULL

    # validate key length
    if (length(value) > .Machine$integer.max) {
        stop(sprintf("'%s' number columns exceeds maximum (%d)",
                     name, .Machine$integer.max))
    }

    # validate key names, compute display string
    names <- names(value)
    if (is.null(names)) {
        nstrs <- rep("", length(value))
    } else {
        i <- which(is.na(names))
        if (length(i) > 0) {
            stop(sprintf("'%s' column name %d is missing", name, i))
        }
        nstrs <- vapply(names, function(nm)
                            if (is.null(nm)) "" else sprintf(" (\"%s\")", nm), "")
    }

    for (i in seq_along(value)) {
        elt <- value[[i]]
        nm <- nstrs[[i]]

        d <- dim(elt)
        if (length(d) > 1) {
            stop(sprintf("'%s' column %d%s is not a vector", name, i, nm))
        }

        # convert to basic types
        if (is.logical(elt)) {
            elt <- as.logical(elt)
        } else if (is.numeric(elt)) {
            if (!is.null(oldClass(elt))) {
                elt <- as.numeric(elt)
            }
        } else if (is.complex(elt)) {
            elt <- as.complex(elt)
        } else {
            elt <- as.character(elt)
            inv <- which(!utf8_valid(elt))
            if (length(inv) > 0) {
                stop(sprintf("'%s' column %d%s cannot be encoded in valid UTF-8 (entry %.0f is invalid)", name, i, nm, inv[[1L]]))
            }
            elt <- as_utf8(elt)
        }
        value[[i]] <- elt
    }
    value
}


as_by_cols <- function(name, value, x)
{
    if (is.null(value)) {
        return(NULL)
    }

    if (!is.numeric(value)) {
        value <- as_character_vector(name, value)
        names <- names(x)
        if (is.null(names) && length(value) > 0) {
            stop(sprintf("'%s' refers to named columns but 'names' is NULL",
                         name))
        }
    }
    if (anyNA(value)) {
        stop(sprintf("'%s' contains NA", name))
    }
    if (!is.numeric(value)) {
        index <- match(value, names)
        i <- which(is.na(index))
        if (length(i) > 0) {
            stop(sprintf("'%s' refers to unknown column \"%s\"", name,
                         value[[i[[1]]]]))
        }
    } else {
        n <- length(x)
        index <- trunc(value)
        i <- which(index < 1 | index > n)
        if (length(i) > 0) {
            stop(sprintf("'%s' refers to column with invalid index (%.0f)",
                         name, index[[i[[1]]]]))
        }
    }
    index
}


as_keys <- function(name, value, x)
{
    if (is.null(value)) {
        return(NULL)
    }

    value <- as_by(name, value, x)

    if (length(value) == 1) {
        i <- which(duplicated(value[[1]]))
        if (length(i) > 0) {
            j <- which(value[[1]] == value[[1]][[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("'%s' has duplicate entries (%.0f and %.0f)",
                         name, j[[1]], j[[2]]))
        }
    } else {
        kv <- key_encode(value)
        i <- which(duplicated(kv))
        if (length(i) > 0) {
            j <- which(kv == kv[[i[[1]]]])
            stopifnot(length(j) > 1)
            stop(sprintf("'%s' has duplicate rows (%.0f and %.0f)",
                         name, j[[1]], j[[2]]))
        }
    }

    value
}


as_key_cols <- function(name, value, x)
{
    if (is.null(value)) {
        return(NULL)
    }

    index <- as_by_cols(name, value, x)
    if (anyDuplicated(index)) {
        stop(sprintf("'%s' contains duplicates", name))
    }
    index
}


### Printing ###

as_chars <- as_nonnegative


as_digits <- function(name, value)
{
    value <- as_nonnegative(name, value)
    if (!is.null(value) && value > 22) {
        stop(sprintf("'%s' must be less than or equal to 22", name))
    }
    value
}


as_justify <- function(name, value)
{
    as_enum(name, value, c("left", "right", "centre", "none"))
}


as_max_print <- as_nonnegative


as_na_print <- function(name, value)
{
    if (is.null(value)) {
        return(NULL)
    }
    value <- as_character_scalar(name, value)
    if (is.na(value)) {
        stop(sprintf("'%s' cannot be NA", name))
    }
    value
}


as_print_gap <- function(name, value)
{
    value <- as_nonnegative(name, value)
    if (!is.null(value) && value > 1024) {
        stop(sprintf("'%s' must be less than or equal to 1024", name))
    }
    value
}


as_rows <- function(name, value)
{
    if (is.null(value)) {
        return(NULL)
    }

    value <- as_integer_scalar(name, value)
    if (is.na(value)) {
        stop(sprintf("'%s' cannot be NA", name))
    }

    value
}

as_cols <- as_rows
