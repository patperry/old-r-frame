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


format_character <- function(x, chars = NULL, na.encode = TRUE, quote = FALSE,
                             na.print = NULL, print.gap = NULL,
                             justify = "none", width = NULL, indent = NULL,
                             line = NULL)
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


format.dataset <- function(x, ..., chars = NULL,
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
        line <- max(12, getOption("width"))
    }

    fmt <- vector("list", length(x))

    for (i in seq_len(nc)) {
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
            fmt[[i]] <- format_character(elt, chars = chars,
                                         na.encode = na.encode,
                                         quote = quote, na.print = na.print,
                                         justify = justify, width = w,
                                         indent = indent, line = line)
        } else { # format others using S3
            fmt[[i]] <- format(elt, ..., chars = chars, na.encode = na.encode,
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


truncate <- function(x, rows = NULL, cols = NULL)
{
    if (is.null(rows)) {
        rows <- 20L
    }
    if (rows < 0) {
        rows <- .Machine$integer.max
    }
    if (is.null(cols)) {
        cols <- 20L
    }
    if (cols < 0) {
        cols <- .Machine$integer.max
    }

    n <- nrow(x)
    trunc <- (n > rows)
    if (trunc) {
        xsub <- x[seq_len(rows), , drop = FALSE]
    } else {
        xsub <- x
    }

    msg <- if (trunc) sprintf("(%d rows total)", n) else NULL
    list(x = xsub, message = msg)
}


print.dataset <- function(x, rows = NULL, cols = NULL, ..., number = TRUE,
                          chars = NULL, digits = NULL, quote = FALSE,
                          na.print = NULL, print.gap = NULL, max = NULL,
                          display = TRUE)
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
        number <- as_option("number", number)
        chars <- as_chars("chars", chars)
        digits <- as_digits("digits", digits)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print_gap", print.gap)
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

    ansi <- output_ansi()
    utf8 <- output_utf8()
    line <- getOption("width")

    if (is.null(na.print)) {
        na.print <- if (quote) "NA" else "<NA>"
    }
    if (is.null(print.gap)) {
        print.gap <- 1
    }

    if (ansi) {
        escapes <- style_faint
        bold  <- function(x) paste0("\x1b[", style_bold, "m",
                                    utf8_encode(x, display = display,
                                                utf8 = utf8),
                                    "\x1b[0m")
        faint <- function(x) paste0("\x1b[", style_faint, "m",
                                    utf8_encode(x, display = display,
                                                utf8 = utf8),
                                    "\x1b[0m")
    } else {
        escapes <- NULL
        bold <- faint <- function(x)
            utf8_encode(x, display = display, utf8 = utf8)
    }
    normal <- function(x, width) {
        x <- utf8_encode(x, quote = quote, escapes = escapes,
                         display = display, utf8 = utf8)
        x[is.na(x)] <- utf8_encode(na.print, width = width, display = display,
                                   utf8 = utf8)
        x
    }

    if (length(x) == 0) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows"), n),
            "\n", sep = "")
        return(invisible(x))
    }

    trunc <- truncate(x, rows, cols)
    xorig <- x
    x <- trunc$x

    gap <- utf8_format("", width = print.gap)
    n <- nrow(x)

    if (number) {
        row_body <- utf8_format(as.character(seq_len(n)),
                                chars = .Machine$integer.max, justify = "left")
        num_width <- max(0, utf8_width(row_body))
        row_head <- utf8_format("", width = num_width)
    } else {
        row_body <- rep("", n)
        num_width <- 0
        row_head <- ""
    }

    keys <- keys(x)
    if (!is.null(keys)) {
        kb <- mapply(function(k, w)
                         utf8_format(k, width = w,
                                     chars = .Machine$integer.max,
                                     justify = "left"),
                     keys, vapply(names(keys), utf8_width, 0),
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
        kh <- mapply(function(n, col)
                        utf8_format(n, width = max(0, utf8_width(col)),
                                    chars = .Machine$integer.max,
                                    justify = "left"),
                     names(keys), kb, USE.NAMES = FALSE)

        kb <- do.call(paste, c(kb, sep = gap))
        kh <- paste(kh, collapse = gap)
        if (nchar(row_head) > 0) {
            row_head <- paste(row_head, kh, sep = gap)
            row_body <- paste(row_body, kb, sep = gap)
        } else {
            row_head <- kh
            row_body <- kb
        }
    }

    row_width <- utf8_width(row_head)
    row_head <- faint(row_head)
    row_body <- faint(row_body)

    if (!is.null(keys)) {
        row_head <- paste0(row_head, gap, " ", gap)
        row_width <- row_width + 1 + 2 * utf8_width(gap)
        row_body <- paste0(row_body, gap, if (utf8) "\u2502" else "|", gap)
    } else if (row_width > 0) {
        row_head <- paste0(row_head, gap)
        row_body <- paste0(row_body, gap)
        row_width <- row_width + utf8_width(gap)
    }

    line <- max(1L, line - row_width)
    fmt <- format.dataset(x, chars = chars, na.encode = FALSE,
                          na.print = na.print, quote = quote,
                          print.gap = print.gap, digits = digits,
                          line = line)

    cols <- as.list.dataset(fmt, flatten = TRUE, path = TRUE)
    path <- attr(cols, "path")
    index <- attr(cols, "index")
    names <- vapply(path, tail, "", n = 1)

    # format columns, using max path width as minimum
    width <- vapply(path, function(p) max(0, utf8_width(p)), 0)
    cols <- mapply(function(col, w)
                       utf8_format(as.character(col), width = w,
                                   chars = .Machine$integer.max,
                                   na.encode = FALSE, na.print = na.print,
                                   quote = quote, justify = "left"),
                   cols, width, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    width <- pmax(width,
                  mapply(function(name, col)
                             col_width(name, col, quote, na.print),
                         names, cols, SIMPLIFY = TRUE, USE.NAMES = FALSE))

    # format names, using column width as minimum
    names <- mapply(function(name, w)
                        utf8_format(name, width = w,
                                    chars = .Machine$integer.max,
                                    justify = "left"),
                    names, width, SIMPLIFY = TRUE, USE.NAMES = FALSE)

    # apply formatting
    names <- bold(names)
    cols <- mapply(normal, cols, width, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    ellipsis <- ifelse(utf8, "\u22ee", ".")

    # wrap columns
    indent <- 0L
    foot_width <- row_width
    start <- 1L
    for (i in seq_along(cols)) {
        indent <- indent + width[[i]] + print.gap

        if (i == length(cols) || indent + width[[i + 1L]] > line) {
            foot_width <- max(foot_width, row_width + indent - print.gap)
            # add padding between previous set of rows
            if (start > 1) {
                cat(faint(ellipsis), "\n", sep="")
            }

            # determine header for nested groups
            depth <- max(1, vapply(index[start:i], length, 0))
            group <- matrix(unlist(lapply(index[start:i], `length<-`, depth)),
                            nrow = depth)
            gname <- matrix(unlist(lapply(path[start:i], `length<-`, depth)),
                            nrow = depth)
            gwidth <- width[start:i]
            m <- i - start + 1

            for (d in seq(from = depth - 1, by = -1, length.out = depth - 1)) {
                j <- 1
                while (j <= m) {
                    g <- group[d, j]
                    if (!is.na(g)) {
                        s <- j
                        while (j < m && group[d,j + 1] %in% g) {
                            j <- j + 1
                        }
                        if (all(is.na(group[d + 1, s:j]))) {
                            k <- d + 1
                            while (k < depth
                                       && all(is.na(group[k+1,s:j]))) {
                                k <- k + 1
                            }
                            group[k,s:j] <- group[d,s:j]
                            group[d,s:j] <- NA
                            gname[k,s:j] <- gname[d,s:j]
                            gname[d,s:j] <- NA
                        }
                    }
                    j <- j + 1
                }
            }

            # print header
            ch <- if (utf8) "\u2550" else "="
            for (d in seq_len(depth - 1)) {
                grp <- group[d,]
                gnm <- gname[d,]
                head <- format("", width = row_width)
                j <- 1
                while (j <= m) {
                    if (j > 1) {
                        head <- paste0(head, gap)
                    }
                    w <- gwidth[[j]]
                    if (is.na(grp[[j]])) {
                        head <- paste0(head, format("", width = w))
                    } else {
                        g <- grp[[j]]
                        nm <- gnm[[j]]
                        # %in% so that this succeeds if NA
                        while (j < m && grp[[j + 1]] %in% g) {
                            j <- j + 1
                            w <- w + print.gap + gwidth[[j]]
                        }
                        wnm <- utf8_width(nm)
                        pad <- max(0, w - wnm)
                        lpad <- floor(pad / 2)
                        rpad <- ceiling(pad / 2)
                        banner <- paste0(paste0(rep(ch, lpad), collapse = ""),
                                         nm,
                                         paste0(rep(ch, rpad), collapse = ""))
                        head <- paste0(head, bold(banner))
                    }
                    j <- j + 1
                }
                cat(head, "\n", sep="")
            }

            head <- paste0(names[start:i], collapse = gap)
            cat(row_head, head, "\n", sep = "")
            if (n > 0) {
                body <- do.call(paste, c(cols[start:i], sep = gap))
                cat(paste0(row_body, body, collapse = "\n"), "\n", sep = "")
            }

            indent <- 0L
            start <- i + 1L
        }
    }

    if (n == 0) {
        cat("(0 rows)\n")
    } else if (!is.null(trunc$message)) {
        foot <- utf8_format(paste0(" ", trunc$message),
                            width = max(0, foot_width - utf8_width(ellipsis)),
                            justify = "right")
        cat(faint(ellipsis), foot, "\n", sep="")
    }

    invisible(xorig)
}
