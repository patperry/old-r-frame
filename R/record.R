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


record_cast <- function(x)
{
    # TODO: optimize in C

    if (!identical(oldClass(x), "record"))
        x <- as.record(x)

    # discard extra attributes if they exist
    names <- names(x)
    nattr <- length(attributes(x))
    if (nattr > 1 + !is.null(names)) {
        attributes(x) <- NULL
        names(x) <- names
        oldClass(x) <- "record"
    }

    x
}


c.record <- function(...)
{
    args <- pairlist(...)
    narg <- length(args)
    argnames <- names(args)

    # optimize common case
    if (narg == 1L && is.null(argnames))
        return(record_cast(args[[1L]]))

    argnames <- arg_record_names(-1, argnames, "argument names")
    names(args) <- NULL
    args <- as.vector(args, "list")

    null <- vapply(args, is.null, FALSE)
    args <- args[!null]
    narg <- length(args)

    if (narg == 0)
        return(NULL)
    else if (narg == 1 && is.null(argnames))

    argnames <- argnames[!null]

    xlist <- lapply(args, as.record)
    nlist <- vapply(xlist, length, 0)
    namelist <- lapply(xlist, names)

    x <- unlist(xlist, FALSE)

    names <- flatten_names(nlist, argnames, namelist)
    if (!is.null(names))
        names(x) <- names

    oldClass(x) <- "record"
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
        record_replace_all(x, value, call)
    else if (is.numeric(i))
        if (any(i < 0L))
            record_replace_except(x, i, value, call)
        else
            record_replace_index(x, length(x), i, value, call)
    else {
        names <- names(x)
        as.record(l, names)
    }
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


record_replace_except <- function(x, i, value, call)
{
    if (anyNA(i))
        i[is.na(i)] <- NA

    if (!is.integer(i))
        i <- trunc(i)

    if (any(i > 0L)) {
        fmt <- "numeric index contains both positive and negative values"
        stop(simpleError(fmt, call))
    }

    n <- length(x)
    record_replace_index(x, n, seq_len(n)[i], value, call)
}


record_replace_index <- function(x, n, i, value, call)
{
    if (n < 2^31)
        i <- as.integer(i)

    ni <- length(i)
    nv <- length(value)

    if (ni != nv && nv != 1L) {
        # NOTE: different behavior from R (which does not count zeroes).
        # This is intentional.
        fmt <- "mismatch: selection length is %.0f, replacement length is %.0f"
        stop(simpleError(sprintf(fmt, ni, nv)))
    }

}





# from ?LongVectors; max is 2^52
RECORD_SIZE_DIGITS <- 52L
RECORD_SIZE_MAX <- 2^RECORD_SIZE_DIGITS


record_lookup <- function(x, i, call)
{
    # TODO: implement in C
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- trunc(i)

        if (anyNA(i))
            i[is.na(i)] <- 0L

        if (!is.integer(i) && !all(i <= RECORD_SIZE_MAX)) {
            invalid <- which(i > RECORD_SIZE_MAX)[[1]]
            fmt <- "index entry %.0f exceeds maximum (%f > 2^%d)"
            stop(simpleError(sprintf(fmt, invalid,
                                     i[[invalid]], RECORD_SIZE_DIGITS),
                             call))
        }
    }
}
