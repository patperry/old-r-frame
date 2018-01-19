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

argname <- function(x) paste0("'", deparse(x), "'")

arg_character_scalar <- function(value, name = argname(substitute(value)),
                                 call = sys.call(-1L), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    value <- arg_character_vector(value, name, call, ...)

    if (length(value) != 1) {
        stop(simpleError(
            sprintf("%s must be a scalar character string", name), call))
    }

    value
}


arg_character_vector <- function(value, name = argname(substitute(value)),
                                 call = sys.call(-1L), ..., utf8 = TRUE)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!(is.null(value) || is.character(value) || is.factor(value)
          || all(is.na(value)))) {
        stop(simpleError(
            sprintf("%s must be a character vector or NULL", name), call))
    }

    value <- as.character(value)

    if (utf8) {
        raw <- value
        value <- tryCatch(as_utf8(raw), error = function(cond) NULL)
        if (is.null(value)) {
            invalid <- which(!utf8_valid(raw))[[1L]]
            stop(simpleError(sprintf("%s, entry %.0f has invalid character encoding",
                                     name, invalid),
                             call))
        }
    }

    value
}


arg_data_frame <- function(value, name = argname(substitute(value)),
                           call = sys.call(-1L), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is.data.frame(value)) {
        stop(simpleError(sprintf("%s is not a valid data frame", name), call))
    }

    value
}


arg_dataset <- function(value, name = argname(substitute(value)),
                        call = sys.call(-1L), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is_dataset(value)) {
        stop(simpleError(sprintf("%s is not a valid dataset object", name), call))
    }

    value
}


arg_function <- function(value, name = argname(substitute(value)),
                         call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is.function(value)) {
        stop(simpleError(sprintf("%s is not a function", name), call))
    }

    value
}


arg_keyset <- function(value, name = argname(substitute(value)),
                       call = sys.call(-1L), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is_keyset(value)) {
        stop(simpleError(sprintf("%s is not a valid keyset object", name), call))
    }

    value
}


arg_list <- function(value, name = argname(substitute(value)),
                     call = sys.call(-1L), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is.list(value)) {
        stop(simpleError(sprintf("%s is not a list", name), call))
    }

    value
}


arg_double_scalar <- function(value, name = argname(substitute(value)),
                              call = sys.call(-1L), ..., allow_null = FALSE)
{
    force(name)

    if (is.null(value)) {
        if (allow_null) {
            return(NULL)
        } else {
            stop(simpleError(sprintf("%s cannot be NULL", name), call))
        }
    }

    if (length(value) != 1) {
        stop(simpleError(sprintf("%s must have length 1", name), call))
    }

    if (!(is.numeric(value) && !is.nan(value) && !is.na(value))) {
        stop(simpleError(sprintf("%s must be a numeric value%s", name,
                                 if (allow_null) " (or NULL)" else ""), call))
    }

    as.double(value)
}


arg_enum <- function(choices, value, name = argname(substitute(value)),
                     call = sys.call(-1), ...)
{
    force(name)

    if (!(is.character(value) && length(value) == 1 && !is.na(value))) {
        stop(simpleError(
            sprintf("%s must be a character string", name), call))
    }

    i <- pmatch(value, choices, nomatch = 0L)
    if (all(i == 0L)) {
        stop(simpleError(
            sprintf("%s must be one of the following: %s", name,
                    paste(dQuote(choices), collapse = ", ")), call))
    }
    i <- i[i > 0L]
    choices[[i]]
}


arg_integer_scalar <- function(value, name = argname(substitute(value)),
                               call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }
    value <- arg_integer_vector(value, name, call, ...)
    if (length(value) != 1) {
        stop(simpleError(sprintf("%s must have length 1", name), call))
    }
    value
}


arg_integer_vector <- function(value, name = argname(substitute(value)),
                               call = sys.call(-1), ..., nonnegative = FALSE)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!(is.numeric(value) || all(is.na(value)))) {
        stop(simpleError(sprintf("%s must be integer-valued", name), call))
    }

    value <- as.integer(value)
    if (nonnegative && any(!is.na(value) & value < 0)) {
        stop(simpleError(sprintf("%s must be non-negative", name), call))
    }

    value
}


arg_nonnegative <- function(value, name = argname(substitute(value)),
                            call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }
    value <- arg_integer_scalar(value, name, call, nonnegative = TRUE, ...)
    if (is.na(value)) {
        stop(simpleError(sprintf("%s cannot be NA", name), call))
    }
    value
}


arg_option <- function(value, name = argname(substitute(value)),
                       call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(FALSE)
    }

    if (!(length(value) == 1 && is.logical(value) && !is.na(value))) {
        stop(simpleError(sprintf("%s must be TRUE or FALSE", name), call))
    }
    as.logical(value)
}

### Dataset ###

arg_names <- function(n, type, value, name = argname(substitute(value)),
                      call = sys.call(-1), ..., allow_na = FALSE, na = NULL,
                      unique = FALSE, utf8 = FALSE)
{
    force(name)

    # NULL names allowed
    if (is.null(value)) {
        return(NULL)
    }

    value <- arg_character_vector(value, name, call, utf8 = utf8, ...)

    if (length(value) != n) {
        stop(simpleError(
            sprintf("mismatch: %s has length %.0f, number of %s is %.0f",
                    name, length(value), type, n), call))
    }

    if (!is.null(na)) {
        value[is.na(value)] <- na
    }

    if (!allow_na) {
        i <- which(is.na(value))
        if (length(i) > 0) {
            stop(simpleError(sprintf("%s cannot contain NA values", name)))
        }
    }

    if (unique) {
        i <- which(duplicated(value))
        if (length(i) > 0) {
            stop(simpleError(sprintf("%s cannot contain duplicate values", name)))
        }
    }


    value
}


arg_by_cols <- function(x, value, name = argname(substitute(value)),
                        call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    if (!is.numeric(value)) {
        value <- arg_character_vector(value, name, call, ...)
        names <- names(x)
        if (is.null(names) && length(value) > 0) {
            stop(simpleError(
                sprintf("%s refers to named columns but 'names' is NULL",
                        name), call))
        }
    }
    if (anyNA(value)) {
        stop(simpleError(sprintf("%s contains NA", name), call))
    }
    if (!is.numeric(value)) {
        index <- match(value, names)
        i <- which(is.na(index))
        if (length(i) > 0) {
            stop(simpleError(
                sprintf("%s refers to unknown column \"%s\"", name,
                        value[[i[[1]]]]), call))
        }
    } else {
        n <- length(x)
        index <- trunc(value)
        i <- which(index < 1 | index > n)
        if (length(i) > 0) {
            stop(simpleError(sprintf(
                "%s refers to column with invalid index (%.0f)",
                name, index[[i[[1]]]]), call))
        }
    }
    index
}


arg_key_cols <- function(x, value, name = argname(substitute(value)),
                         call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    index <- arg_by_cols(x, value, name, call, ...)

    if (anyDuplicated(index)) {
        stop(simpleError(sprintf("%s contains duplicates", name), call))
    }

    index
}


### Printing ###

arg_chars <- arg_nonnegative


arg_digits <- function(value, name = argname(substitute(value)),
                       call = sys.call(-1), ...)
{
    force(name)

    value <- arg_nonnegative(value, name, call, ...)

    if (!is.null(value) && value > 22) {
        stop(simpleError(
            sprintf("%s must be less than or equal to 22", name), call))
    }

    value
}


arg_justify <- function(value, name = argname(substitute(value)),
                        call = sys.call(-1), ...)
{
    force(name)
    arg_enum(c("left", "right", "centre", "none"), value, name, call, ...)
}


arg_max_print <- arg_nonnegative


arg_na_print <- function(value, name = argname(substitute(value)),
                         call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    value <- arg_character_scalar(value, name, call, ...)

    if (is.na(value)) {
        stop(simpleError(sprintf("%s cannot be NA", name), call))
    }

    value
}


arg_print_gap <- function(value, name = argname(substitute(value)),
                          call = sys.call(-1), ...)
{
    force(name)

    value <- arg_nonnegative(value, name, call, ...)

    if (!is.null(value) && value > 1024) {
        stop(simpleError(
            sprintf("%s must be less than or equal to 1024", name), call))
    }

    value
}


arg_rows <- function(value, name = argname(substitute(value)),
                     call = sys.call(-1), ...)
{
    force(name)

    if (is.null(value)) {
        return(NULL)
    }

    value <- arg_integer_scalar(value, name, call, ...)
    if (is.na(value)) {
        stop(simpleError(sprintf("%s cannot be NA", name), call))
    }

    value
}

arg_cols <- arg_rows


arg_slice <- function(nkey, keynames, slice, call = sys.call(-1))
{
    if (!is.list(slice) && !is.null(slice)) {
        stop(simpleError(sprintf("slice must be a list or NULL, not \"%s",
                                 class(slice)[[1L]]), call))
    }

    n <- length(slice)
    if (n == 0L) {
        return(slice)
    }

    for (i in seq_len(n)) {
        value <- slice[[i]]
        r <- length(dim(value))
        if (r > 1L) {
            stop(simpleError(sprintf(
                 "cannot slice with rank-%.0f array for key value", r), call))
        }
    }

    names <- names(slice)
    if (is.null(names)) {
        if (n > nkey) {
            stop(simpleError(sprintf(
                 "'slice' length (%.0f) exceeds number of keys (%.0f)",
                 n, nkey), call))

        }
        return(slice)
    }

    if (is.null(keynames)) {
        stop(simpleError(
             "cannot use named slice when key names are NULL", call))
    }

    if (identical(names, keynames)) {
        # TODO: require key names to be unique for this to be a valid
        # optimization
        return(slice)
    }

    index <- vector("list", nkey)
    for (i in seq_len(n)) {
        name <- names[[i]]
        value <- slice[[i]]

        if (nzchar(name)) {
            j <- match(name, keynames)
            if (is.na(j)) {
                stop(simpleError(sprintf("unknown key name \"%s\"", name),
                                 call))
            }
        } else if (i > nkey) {
            stop(simpleError(sprintf(
                 "slice index (%.0f) exceeds number of keys (%.0f)",
                 i, nkey), call))
        } else {
            j <- i
        }

        if (is.null(index[[j]])) {
            index[[j]] <- value
        } else if (!is.null(value)) {
            index[[j]] <- c(index[[j]], value)
        }
    }

    index
}
