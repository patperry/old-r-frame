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
    as.record(x)    
}


as.record <- function(x, names = NULL, ...)
{
    UseMethod("as.record")
}


as.record.record <- function(x, names = NULL, ...)
{
    if (!is.record(x))
        x <- as.record(x, ...)

    if (!is.null(names))
        names(x) <- names

    x
}


as.record.default <- function(x, names = NULL, ...)
{
    x <- as.list(x)
    as.record(x, names)
}





as.record.list <- function(x, names = NULL, ..., flat = FALSE)
{
    n <- length(x)
    names <- if (is.null(names))
        arg_record_names(n, names(x))
    else arg_record_names(n, names)


    if (flat) {
        l <- lapply(x, as.record, flat = TRUE)

        if (!is.null(names)) {
            # can't 'do.call(c.record, l)' because 'names' might contain NA or ""
            ns <- vapply(l, length, 0)
            ntot <- sum(ns)
        }

        names(l) <- names
    }

    attributes(x) <- NULL
    class(x) <- "record"
    attr(x, "names") <- names

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
    args <- list(...)
    argnames <- arg_record_names(-1, names(args), "argument names")
    names(args) <- NULL

    null <- vapply(args, is.null, FALSE)
    args <- args[!null]
    narg <- length(args)

    if (narg == 0)
        return(NULL)

    argnames <- argnames[!null]

    xlist <- lapply(args, as.record)
    nlist <- vapply(xlist, length, 0)
    namelist <- lapply(xlist, names)

    x <- unlist(xlist, FALSE)

    names <- flatten_names(nlist, argnames, namelist)
    if (!is.null(names))
        attr(x, "names") <- names

    class(x) <- "record"
    x
}


qualify_names <- function(n, prefix = NULL, names = NULL)
{
    if (is.null(prefix) || !nzchar(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}


flatten_names <- function(nlist, prefixlist = NULL, nameslist = NULL)
{
    result <- NULL

    off <- 0
    for (i in seq_along(nlist)) {

        n <- nlist[[i]]
        names <- qualify_names(n, prefixlist[[i]], nameslist[[i]])

        if (!is.null(names)) {
            if (is.null(result))
                result <- character(sum(nlist))

            if (n > 0)
                result[(off + 1):(off + n)] <- names
        }

        off <- off + n
    }

    result
}


is.record <- function(x)
{
    inherits(x, "record")
}


`names<-.record` <- function(x, value)
{
    names <- arg_record_names(length(x), value)
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
    as.record(l, names)
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
