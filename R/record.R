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


record <- function(...)
{
    args <- as.list(substitute(list(...)))[-1]
    x <- list(...)
    n <- length(x)

    names <- names(x)
    if (is.null(names))
        names <- character(n)

    for (i in seq_len(n)) {
        if (!nzchar(names[[i]])) {
            names[[i]] <- deparse(args[[i]], 500)[[1]]
        }
    }

    names(x) <- names
    as_record(x)    
}


as_record <- function(x, names = NULL, ...)
{
    UseMethod("as_record")
}


as_record.record <- function(x, names = NULL, ...)
{
    if (!is_record(x))
        x <- as_record(x)

    if (!is.null(names))
        names(x) <- names

    x
}


as_record.default <- function(x, names = NULL, ...)
{
    x <- as.list(x)
    as_record(x, names)
}


as_record.list <- function(x, names = NULL, ...)
{
    if (is.null(names))
        names <- names(x)

    attributes(x) <- NULL
    class(x) <- "record"
    names(x) <- names

    x
}


as.list.record <- function(x, names = NULL, ...)
{
    if (is.null(names))
        names <- names(x)

    attributes(x) <- NULL
    names(x) <- names

    x
}


as.vector.record <- function(x, mode = "any")
{
    as.vector(as.list(x), mode)
}


c.record <- function(...)
{
    dots <- list(...)
    null <- vapply(dots, is.null, FALSE)
    args <- dots[!null]
    narg <- length(args)

    if (narg == 0)
        return(NULL)

    argnames <- names(args)
    has_argnames <- !is.null(argnames)

    xs <- lapply(args, as_record)
    ns <- vapply(xs, length, 0)

    n <- sum(ns)
    l <- vector("list", n)
    has_names <- FALSE

    off <- 0
    for (i in seq_len(narg)) {
        xi <- xs[[i]]
        ni <- ns[[i]]

        dst <- off + seq_len(ni)
        l[dst] <- unname(as.list(xi))

        namesi <- if (has_argnames)
            qualify_names(argnames[[i]], ni, names(xi))
        else names(xi)

        if (!is.null(namesi)) {
            if (!has_names) {
                names <- character(n)
                has_names <- TRUE
            }

            names[dst] <- namesi
        }

        off <- off + ni
    }

    x <- as_record(l)

    # optimization: no need to validate since argument names
    # are valid in the user's native locale, hence valid UTF-8
    if (has_names)
        attr(x, "names") <- names

    x
}


qualify_names <- function(prefix, n, names)
{
    if (!nzchar(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}


is_record <- function(x)
{
    inherits(x, "record")
}


`names<-.record` <- function(x, value)
{
    names <- arg_record_names(x, value)
    attr(x, "names") <- names
    x
}


`$.record` <- function(x, name)
{
    x[[name]]
}


`$<-.record` <- function(x, name, value)
{
    x[[name]] <- value
}


`[[.record` <- function(x, i, exact = TRUE)
{
    # TODO: implement in C

    if (!identical(exact, TRUE))
        warning("'exact' argument is ignored")

    if (missing(i))
        stop("missing index")

    n <- length(i)
    i1 <- arg_record_index1(n, i)

    x1 <- x[i1]
    entry <- .subset2(x1, 1)

    if (n > 1)
        entry[[ i[-1] ]]
    else entry
}



`[[<-.record` <- function(x, i, value)
{
    # TODO: implement in C

    if (missing(i))
        stop("missing index")

    n <- length(i)
    i1 <- arg_record_index1(n, i)

    if (n == 1) {
        x[i1] <- value
    } else {
        x[[i1]][[ i[-1] ]] <- value
    }
}


`[.record` <- function(x, i)
{
    i <- arg_record_subset(x, i)
    if (is.null(i)) {
        x
    } else {
        y <- .subset(x, i)
        class(y) <- "record"
        y
    }
}


`[<-.record` <- function(x, i, value)
{
    i <- arg_record_subset(x, i)

    if (is.null(value))
        record_delete(x, i)
    else record_replace(x, i, value)
}


record_delete <- function(x, i)
{
    if (is.null(i))
        return(record_delete_all(x))

    if (is.character(i))
        i <- match(i, names(x))

    class(x) <- NULL
    x[i] <- NULL
    class(x) <- "record"

    y
}


record_delete_all <- function(x)
{
    class(x) <- NULL
    x[] <- NULL
    class(x) <- "record"
    x
}


record_replace <- function(x, i, value, call = sys.call(-1))
{
    if (is.null(i))
        return(record_replace_all(x, value, call))

    names <- names(x)
    as_record(l, names)
}


record_replace_all <- function(x, value, call)
{
    n <- length(x)
    nv <- length(value)

    if (nv == n) {
        if (is.object(value))
            value <- as.list(value)
    } else if (nv == 1) {
        if (is.object(value))
            value <- value[[1]]
        value <- rep_len(value, n)
    } else {
        fmt <- "mismatch: replacement length is %.0f, selection length is %.0f"
        stop(simpleError(sprintf(fmt, nv, n), call))
    }

    class(x) <- NULL
    x[] <- value
    class(x) <- "record"

    x
}


RECORD_SIZE_MAX <- 2^53 # others can't be stored as double

record_lookup <- function(x, i, call)
{
    if (is.numeric(i)) {
        invalid <- !(1 <= i & i < RECORD_SIZE_MAX)
        if (any(invalid)) {
            j <- which.max(invalid)
            fmt <- "index entry %.0f is invalid (%s)"
            stop(simpleError(sprintf(fmt, j, i[[j]])))
        }

    } else if (is.character(i)) {
        match(i, names(x))
    }
}
