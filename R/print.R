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

new_format_control <- function(chars = NULL, digits = NULL,
                               na.encode = TRUE, quote = FALSE,
                               na.print = NULL, print.gap = NULL,
                               justify = "none", width = NULL,
                               display = TRUE, line = NULL)
{
    control <- list()
    control$chars <- as_chars("chars", chars)
    control$digits <- as_digits("digits", digits)
    control$na.encode <- as_option("na.encode", na.encode)
    control$quote <- as_option("quote", quote)
    control$na.print <- as_na_print("na.print", na.print)
    control$print.gap <- as_print_gap("print.gap", print.gap)
    control$justify <- as_justify("justify", justify)
    control$width <- as_integer_scalar("width", width)
    control$display <- as_option("display", display)
    control$line <- as_integer_scalar("line", line, nonnegative = TRUE)
    control$ansi <- output_ansi()
    control$utf8 <- output_utf8()

    if (is.null(control$na.print)) {
        control$na.print <- if (control$quote) "NA" else "<NA>"
    }
    if (is.null(control$print.gap)) {
        control$print.gap <- 1L
    }
    if (is.null(control$width)) {
        control$width <- 0L
    }
    if (is.null(control$line)) {
        control$line <- getOption("width")
    }

    control$banner    <- if (control$utf8) "\u2550" else "="
    control$ellipsis  <- if (control$utf8) "\u2026" else "..."
    control$vellipsis <- if (control$utf8) "\u22ee" else "."
    control$vline     <- if (control$utf8) "\u2502" else "|"

    control
}


new_format_style <- function(control)
{
    if (control$ansi) {
        escapes <- style_faint
        bold  <- function(x) paste0("\x1b[", style_bold, "m",
                                    utf8_encode(x, display = control$display,
                                                utf8 = control$utf8),
                                    "\x1b[0m")
        faint <- function(x) paste0("\x1b[", style_faint, "m",
                                    utf8_encode(x, display = control$display,
                                                utf8 = control$utf8),
                                    "\x1b[0m")
    } else {
        escapes <- NULL
        bold <- faint <- function(x)
            utf8_encode(x, display = control$display, utf8 = control$utf8)
    }

    normal <- function(x, width) {
        x <- utf8_encode(x, quote = control$quote, escapes = control$escapes,
                         display = control$display, utf8 = control$utf8)
        x[is.na(x)] <- utf8_encode(control$na.print, width = width,
                                   display = control$display,
                                   utf8 = control$utf8)
        x
    }

    list(normal = normal, bold = bold, faint = faint)
}


col_width <- function(name, x, control)
{
    stopifnot(length(dim(x)) <= 1)

    n <- utf8_width(name)
    w <- max(0, utf8_width(x, quote = control$quote), na.rm = TRUE)
    if (anyNA(x)) {
        naw <- utf8_width(control$na.print)
        w <- max(w, naw)
    }

    max(n, w)
}


col_widow <- function(name, x, control, indent)
{
    if (length(dim(x)) <= 1) {
        w <- col_width(name, x, control)
        indent <- indent + w + control$print.gap
        if (indent > control$line + control$print.gap) {
            indent <- w + control$print.gap
        }
    } else if (is.data.frame(x)) {
        names <- names(x)
        for (i in seq_along(x)) {
            indent <- col_widow(names[[i]], x[[i]], control, indent)
        }
    } else {
        names <- colnames(x)
        if (is.null(names)) {
            names <- rep("", ncol(x))
        }
        for (j in seq_len(ncol(x))) {
            indent <- col_widow(names[[j]], x[, j, drop = TRUE],
                                control, indent)
        }
    }
    indent
}


format_vector <- function(x, ..., control = NULL, indent = NULL,
                          sections = NULL)
{
    chars <- control$chars
    if ((stretch <- is.null(chars))) {
        ellipsis <- utf8_width(control$ellipsis)
        quotes <- if (control$quote) 2 else 0

        chars_min <- 24
        chars_max <- max(chars_min, control$line - ellipsis - quotes)
        chars <- chars_max - indent
        if (chars < chars_min) {
            chars <- chars_max
        }
    }

    cl <- class(x)
    if (is.character(x) && (identical(cl, "character")
                            || identical(cl, "AsIs"))) {
        utf8_format(x, chars = chars, justify = control$justify,
                    width = control$width, na.encode = control$na.encode,
                    quote = control$quote, na.print = control$na.print)
    } else {
        format(x, ..., chars = chars, na.encode = control$na.encode,
               quote = control$quote, na.print = control$na.print,
               print.gap = control$print.gap, justify = control$justify,
               width = control$width, indent = indent, line = control$line,
               sections = sections)
    }
}


format_matrix <- function(x, ..., control = control, indent = NULL,
                          sections = NULL)
{
    format(x, ..., chars = control$chars, na.encode = control$na.encode,
           quote = control$quote, na.print = control$na.print,
           print.gap = control$print.gap, justify = control$justify,
           width = control$width, indent = indent, line = control$line,
           sections = sections)
}


format.dataset <- function(x, ..., chars = NULL,
                           na.encode = TRUE, quote = FALSE, na.print = NULL,
                           print.gap = NULL, justify = "none", width = NULL,
                           indent = NULL, line = NULL, sections = NULL)
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is_dataset(x)) {
        stop("argument is not a valid dataset")
    }

    with_rethrow({
        control <- new_format_control(chars = chars, na.encode = na.encode,
                                      quote = quote, na.print = na.print,
                                      print.gap = print.gap, justify = justify,
                                      width = width, line = line)
        indent <- as_integer_scalar("indent", indent, nonnegative = TRUE)
        sections <- as_integer_scalar("sections", sections)
    })

    if (is.null(indent)) {
        indent <- 0L
    }
    if (is.null(sections) || sections < 0) {
        sections <- .Machine$integer.max
    }

    nr <- nrow(x)
    nc <- ncol(x)
    names <- names(x)
    keys <- keys(x)

    fmt <- vector("list", length(x))

    for (i in seq_len(nc)) {
        if (sections == 0) {
            fmt <- fmt[1:i]
            names <- names[1:i]
            fmt[[i]] <- rep(control$ellipsis, nr)
            names[[i]] <- control$ellipsis
            break
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
        }

        # format the column
        if (length(dim(elt)) <= 1) {
            fmt[[i]] <- format_vector(elt, ...,
                                      control = `[[<-`(control, "width", w),
                                      indent = indent, sections = sections)
        } else {
            fmt[[i]] <- format_matrix(elt, ...,
                                      control = `[[<-`(control, "width", w),
                                      indent = indent, sections = sections)
        }

        if (length(d) <= 1 && right) {
            w <- col_width(names[[i]], fmt[[i]], control)
            names[[i]] <- utf8_format(names[[i]],
                                      chars = .Machine$integer.max,
                                      justify = "right", width = w)
        }

        indent <- col_widow(names[[i]], fmt[[i]], control, indent)

        # wrap to next line
        if (indent >= control$line) {
            indent <- 0L
            sections <- sections - 1L
        }
    }

    names(fmt) <- names
    x <- as_dataset(fmt)
    keys(x) <- keys
    x
}


truncate <- function(x, rows = NULL)
{
    if (is.null(rows)) {
        rows <- 20L
    }
    if (rows < 0) {
        rows <- .Machine$integer.max
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


print.dataset <- function(x, rows = NULL, sections = 1L, ..., number = TRUE,
                          chars = NULL, digits = NULL, quote = FALSE,
                          na.print = NULL, print.gap = NULL, display = TRUE)
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    with_rethrow({
        rows <- as_rows("rows", rows)
        sections <- as_integer_scalar("sections", sections)
        number <- as_option("number", number)
        control <- new_format_control(chars = chars, digits = digits,
                                      quote = quote, na.print = na.print,
                                      print.gap = print.gap, display = display)
    })

    style <- new_format_style(control)

    if (length(x) == 0) {
        n <- nrow(x)
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows"), n),
            "\n", sep = "")
        return(invisible(x))
    }

    trunc <- truncate(x, rows)
    xorig <- x
    x <- trunc$x
    n <- nrow(x)

    gap <- utf8_format("", width = control$print.gap)

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
    row_head <- style$faint(row_head)
    row_body <- style$faint(row_body)

    if (!is.null(keys)) {
        row_head <- paste0(row_head, gap, " ", gap)
        row_width <- row_width + 1 + 2 * utf8_width(gap)
        row_body <- paste0(row_body, gap, control$vline, gap)
    } else if (row_width > 0) {
        row_head <- paste0(row_head, gap)
        row_body <- paste0(row_body, gap)
        row_width <- row_width + utf8_width(gap)
    }

    line <- max(1L, control$line - row_width)
    fmt <- format.dataset(x, chars = control$chars, sections = sections,
                          na.encode = FALSE, na.print = control$na.print,
                          quote = control$quote, print.gap = control$print.gap,
                          digits = control$digits, line = line)

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
                             col_width(name, col, control),
                         names, cols, SIMPLIFY = TRUE, USE.NAMES = FALSE))

    # format names, using column width as minimum
    names <- mapply(function(name, w)
                        utf8_format(name, width = w,
                                    chars = .Machine$integer.max,
                                    justify = "left"),
                    names, width, SIMPLIFY = TRUE, USE.NAMES = FALSE)

    # apply formatting
    names <- style$bold(names)
    cols <- mapply(style$normal, cols, width,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)

    # wrap columns
    indent <- 0L
    foot_width <- row_width
    start <- 1L
    for (i in seq_along(cols)) {
        indent <- indent + width[[i]] + control$print.gap

        if (i == length(cols) || indent + width[[i + 1L]] > line) {
            foot_width <- max(foot_width,
                              row_width + indent - control$print.gap)
            # add padding between previous set of rows
            if (start > 1) {
                cat(style$faint(control$vellipsis), "\n", sep="")
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
                            w <- w + control$print.gap + gwidth[[j]]
                        }
                        wnm <- utf8_width(nm)
                        pad <- max(0, w - wnm)
                        lpad <- floor(pad / 2)
                        rpad <- ceiling(pad / 2)
                        banner <- paste0(paste0(rep(control$banner, lpad),
                                                collapse = ""),
                                         nm,
                                         paste0(rep(control$banner, rpad),
                                                collapse = ""))
                        head <- paste0(head, style$bold(banner))
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
        cat(style$faint(ellipsis), foot, "\n", sep="")
    }

    invisible(xorig)
}
