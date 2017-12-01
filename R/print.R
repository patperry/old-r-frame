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

col_width <- function(name, x, quote, na.print)
{
    stopifnot(length(dim(x)) <= 1)

    n <- utf8_width(name)

    w <- max(0, utf8_width(x, quote = quote), na.rm = TRUE)
    if (anyNA(x)) {
        naw <- utf8_width(na.print)
        w <- max(w, naw)
    }

    max(n, w)
}


col_widow <- function(name, x, quote, na.print, print.gap, indent, line)
{
    if (length(dim(x)) <= 1) {
        w <- col_width(name, x, quote, na.print)
        indent <- indent + w + print.gap
        if (indent > line + print.gap) {
            indent <- w + print.gap
        }
    } else if (is.data.frame(x)) {
        names <- names(x)
        for (i in seq_along(x)) {
            indent <- col_widow(names[[i]], x[[i]], quote, na.print,
                                print.gap, indent, line)
        }
    } else {
        names <- colnames(x)
        if (is.null(names)) {
            names <- rep("", ncol(x))
        }
        for (j in seq_len(ncol(x))) {
            indent <- col_widow(names[[j]], x[, j, drop = TRUE], quote,
                                na.print, print.gap, indent, line)
        }
    }
    indent
}


format_character <- function(x, cols = NULL, chars = NULL,
                             na.encode = TRUE, quote = FALSE, na.print = NULL,
                             print.gap = NULL, justify = "none", width = NULL,
                             indent = NULL, line = NULL)
{
    if ((stretch <- is.null(chars))) {
        utf8 <- output_utf8()
        ellipsis <- if (utf8) 1 else 3
        quotes <- if (quote) 2 else 0

        chars_min <- 24
        chars_max <- max(chars_min, line - ellipsis - quotes)
        chars <- chars_max - indent
        if (chars < chars_min) {
            chars <- chars_max
        }
    }

    # TODO: handle matrix
    utf8_format(x, chars = chars, justify = justify,
                width = width, na.encode = na.encode,
                quote = quote, na.print = na.print)
}


format.dataset <- function(x, cols = NULL, ..., number = TRUE, chars = NULL,
                           na.encode = TRUE, quote = FALSE, na.print = NULL,
                           print.gap = NULL, justify = "none", width = NULL,
                           indent = NULL, line = NULL)
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }

    with_rethrow({
        cols <- as_cols("cols", cols)
        number <- as_option("number", number)
        chars <- as_chars("chars", chars)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print.gap", print.gap)
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        indent <- as_integer_scalar("indent", indent, nonnegative = TRUE)
        line <- as_integer_scalar("line", line, nonnegative = TRUE)
    })

    nr <- nrow(x)
    nc <- ncol(x)
    names <- names(x)
    keys <- keys(x)

    if (is.null(na.print)) {
        na.print <- if (quote) "NA" else "<NA>"
    }
    if (is.null(print.gap)) {
        print.gap <- 1L
    }
    if (is.null(width)) {
        width <- 0L
    }
    if (is.null(indent)) {
        indent <- 0L
    }
    if (is.null(line)) {
        num_width <- if (!number || nr == 0) 0 else {
            1 + floor(log10(nr)) + print.gap
        }
        key_width <- if (is.null(keys)) 0 else {
            (sum(mapply(col_width, names(keys), as.list(keys),
                        MoreArgs = list(quote = quote, na.print = na.print)))
             + length(keys) * print.gap
             + 1 + print.gap)
        }
        screen_width <- getOption("width")
        line <- max(24, screen_width - (num_width + key_width))
    }

    fmt <- vector("list", length(x))

    for (i in seq_len(nc)) {
        if (!is.null(cols) && cols == 0) {
             break
        }

        # wrap to next line
        if (indent >= line) {
            indent <- 0L
        }

        elt <- x[[i]]
        cl <- class(elt)
        d <- dim(elt)
        right <- (is.numeric(elt) || is.complex(elt))

        # determine the minimum element width
        w <- max(width, utf8_width(names[[i]]))

        # convert factor to character
        if (is.factor(elt) && (identical(cl, "factor")
                               || identical(cl, c("AsIs", "factor")))) {
            elt <- structure(as.character(elt), names = names(elt),
                             dim = dim(elt), dimnames = dimnames(elt))
            cl <- class(elt)
        }

        # format character specially
        if (is.character(elt) && (identical(cl, "character")
                                  || identical(cl, "AsIs"))) {
            fmt[[i]] <- format_character(elt, cols = cols, chars = chars,
                                         na.encode = na.encode,
                                         quote = quote, na.print = na.print,
                                         justify = justify, width = w,
                                         indent = indent, line = line)
        } else { # format others using S3
            fmt[[i]] <- format(elt, cols = cols, ..., number = FALSE,
                               chars = chars, na.encode = na.encode,
                               quote = quote, na.print = na.print,
                               print.gap = print.gap, justify = justify,
                               width = w, indent = indent, line = line)
        }

        if (length(d) <= 1 && right) {
            w <- col_width(names[[i]], fmt[[i]], quote = quote,
                           na.print = na.print)

            names[[i]] <- utf8_format(names[[i]],
                                      chars = .Machine$integer.max,
                                      justify = "right", width = w)
        }

        indent <- col_widow(names[[i]], fmt[[i]], quote = quote,
                            na.print = na.print, print.gap = print.gap,
                            indent = indent, line = line)
    }

    names(fmt) <- names
    x <- as_dataset(fmt)
    keys(x) <- keys
    x
}



print.dataset <- function(x, rows = NULL, cols = NULL, ..., chars = NULL,
                          digits = NULL, quote = FALSE, na.print = NULL,
                          print.gap = NULL, max = NULL, display = TRUE)
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    n <- nrow(x)
    nc <- length(x)

    with_rethrow({
        rows <- as_rows("rows", rows)
        cols <- as_cols("cols", cols)
        chars <- as_chars("chars", chars)
        digits <- as_digits("digits", digits)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print_gap", print.gap)
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

    if (is.null(rows)) {
        rows <- 20L
    }
    if (rows < 0) {
        rows <- .Machine$integer.max
    }

    if (length(x) == 0) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows"), n),
            "\n", sep = "")
        return(invisible(x))
    } else if (n == 0 && is.null(names(x))) {
        cat(sprintf(ngettext(nc, "data frame with %d column and 0 rows",
                             "data frame with %d columns and 0 rows"), nc),
            "\n", sep = "")
        return(invisible(x))
    }

    trunc <- (!is.null(rows) && n > rows)
    if (trunc) {
        xsub <- x[seq_len(rows), , drop = FALSE]
    } else {
        xsub <- x
    }

    fmt <- format.dataset(xsub, cols = cols, chars = chars,
                          na.encode = FALSE, na.print = na.print,
                          quote = quote, print.gap = print.gap,
                          digits = digits)
    m <- as.matrix(fmt)
    storage.mode(m) <- "character"
    rownames(m) <- seq_len(nrow(m))

    utf8_print(m, chars = .Machine$integer.max, quote = quote,
               na.print = na.print, print.gap = print.gap,
               right = FALSE, max = max, names = style_bold,
               rownames = style_faint, escapes = style_faint,
               display = display)

    if (n == 0) {
        cat("(0 rows)\n")
    } else if (trunc) {
        name_width <- max(0, utf8_width(rownames(m)))

        ellipsis <- ifelse(output_utf8(), "\u22ee", ".")
        ellipsis <- substr(ellipsis, 1, name_width)
        gap <- if (is.null(print.gap)) 1 else print.gap
        space <- format(ellipsis, width = name_width + gap)
        if (output_ansi()) {
            space <- paste0("\x1b[", style_faint, "m", space, "\x1b[0m")
        }
        cat(space, sprintf("(%d rows total)\n", n), sep = "")
    }

    invisible(x)
}
