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


is.record <- function(x)
{
    inherits(x, "record")
}


as.record <- function(x, names = NULL, ...)
{
    UseMethod("as.record")
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
        record_concat(l, unsafe = TRUE)
    } else {
        attributes(x) <- NULL
        names(x) <- names
        oldClass(x) <- "record"
        x
    }
}


as.record.record <- function(x, names = NULL, ...)
{
    if (!missing(names) && !is.null(names))
        names(x) <- names

    x
}


as.vector.record <- function(x, mode = "any")
{
    if (mode == "any")
        x
    else NextMethod()
}


as_vector_mode <- function(mode, x, names = NULL)
{
    if (is.null(names))
        names <- names(x)
    x <- as.vector(x, mode)
    names(x) <- names
    x
}


as.complex.record <- function(x, names = NULL, ...)
    as_vector_mode("complex", x, names)

as.double.record <- function(x, names = NULL, ...)
    as_vector_mode("double", x, names)

as.integer.record <- function(x, names = NULL, ...)
    as_vector_mode("integer", x, names)

as.list.record <- function(x, names = NULL, ...)
    as_vector_mode("list", x, names)

as.logical.record <- function(x, names = NULL, ...)
    as_vector_mode("logical", x, names)


as.character.record <- function(x, names = NULL, ...) {
    if (!anyNA(x))
        return(as_vector_mode("character", x, names))

    # Work around bug (?): as.character(list(NA)) is "NA" as of R 3.4.3
    # https://stat.ethz.ch/pipermail/r-devel/2018-January/075388.html

    na <- which(is.na(x))
    y <- as_vector_mode("character", x, names)
    y[na] <- NA_character_
    y
}


c.record <- function(...)
{
    args <- pairlist(...)
    narg <- length(args)

    if (narg == 1 && is.null(names(args)))
        record_cast(args[[1]])
    else
        record_concat(as.list(args))
}




record_cast <- function(x)
{
    if (!identical(oldClass(x), "record"))
        x <- as.record(x)

    names <- names(x)
    nattr <- length(attributes(x))

    if (nattr > 1 + !is.null(names)) {
        attributes(x) <- NULL
        names(x) <- names
        oldClass(x) <- "record"
    }

    x
}


record_concat <- function(xlist, names = NULL, unsafe = FALSE)
{
    if (is.null(names))
        names <- names(xlist)

    if (!unsafe)
        names <- arg_record_names(-1, names, "component names")

    m <- length(xlist)
    nlist <- numeric(m)
    nameslist <- vector("list", m)
    empty <- TRUE

    for (i in seq_len(m)) {
        xi <- xlist[[i]]
        if (is.null(xi))
            next

        if (!is.record(xi)) {
            xi <- as.record(xi)
            xlist[[i]] <- xi
        }

        empty <- FALSE
        nlist[[i]] <- length(xi)
        namesi <- names(xi)
        if (!is.null(namesi))
            nameslist[[i]] <- namesi
    }

    if (empty)
        return(NULL)

    n <- sum(nlist)
    x <- vector("list", n)

    off <- 0
    for (i in seq_len(m)) {
        ni <- nlist[[i]]

        if (ni == 0)
            next

        x[(off + 1):(off + ni)] <- xlist[[i]]
        off <- off + ni
    }

    names(x) <- flatten_names(nlist, nameslist, names)
    oldClass(x) <- "record"
    x
}



qualify_names <- function(n, names = NULL, prefix = NULL)
{
    if (is.null(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}


flatten_names <- function(nlist, nameslist = NULL, prefixlist = NULL)
{
    result <- NULL

    off <- 0
    for (i in seq_along(nlist)) {
        n <- nlist[[i]]
        names <- qualify_names(n, nameslist[[i]], prefixlist[[i]])

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

    x
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
        as.record(x, names)
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
