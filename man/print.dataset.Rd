\name{print.dataset}
\title{Data Set Printing}
\alias{format.dataset}
\alias{print.dataset}
\description{
Printing and formatting data sets.
}
\usage{
\method{print}{dataset}(x, rows = NULL, wrap = NULL, ..., number = NULL,
      chars = NULL, digits = NULL, quote = FALSE, na.print = NULL,
      print.gap = NULL, display = TRUE)

\method{format}{dataset}(x, rows = -1L, wrap = -1L, ..., chars = NULL,
      na.encode = TRUE, quote = FALSE, na.print = NULL, print.gap = NULL,
      justify = "none", width = NULL, indent = NULL, line = NULL,
      meta = FALSE)
}
\arguments{
    \item{x}{data frame object to print or format.}

    \item{rows}{integer scalar giving the maximum number of rows to print
        before truncating the output. A negative or missing value indicates
        no upper limit. Use \code{NULL} to take the value from
        \code{getOption("frame.rows")}.}

    \item{wrap}{integer scalar giving the maximum number of times to wrap
        columns to a new set of lines before truncating the output.
        A negative or missing value indicates no upper limit. Use \code{NULL}
    to take the value from \code{getOption("frame.wrap"}).}

    \item{number}{logical scalar indicating whether to print row numbers
        for the output, or \code{NULL} to number when \code{keys(x)} is
        \code{NULL}.}

    \item{chars}{maximum number of character units to display; see
        \code{\link{utf8_format}}.}

    \item{digits}{minimal number of significant digits; see
        \code{\link{print.default}}.}

    \item{quote}{logical scalar indicating whether to put surrounding
        double-quotes (\code{'"'}) around character strings and escape
        internal double-quotes.}

    \item{na.print}{character string (or \code{NULL}) indicating
        the encoding for \code{NA} values. Ignored when
        \code{na.encode} is \code{FALSE}.}

    \item{print.gap}{non-negative integer (or \code{NULL}) giving the
        number of spaces in gaps between columns; set to \code{NULL}
        or \code{1} for a single space.}

    \item{display}{logical scalar indicating whether to optimize the
        printing for display, not byte-for-byte data transmission;
        see \code{utf8_encode}.}

    \item{na.encode}{logical scalar indicating whether to encode
         \code{NA} values as character strings.}

    \item{justify}{justification; one of \code{"left"}, \code{"right"},
        \code{"centre"}, or \code{"none"}. Can be abbreviated.}

    \item{width}{integer scalar specifying the minimum field width,
        or \code{NULL} for no minimum.}

    \item{indent}{integer scalar specifying the indent in spaces preceding
        the formatted object, or \code{NULL} for none.}

    \item{line}{integer scalar specifying the line width in spaces, or
       \code{NULL} for \code{getOption("width")}.}

    \item{meta}{logical scalar indicating whether or not to include
        meta-formatting attributes in the result.}

    \item{...}{further arguments passed to or from other methods.}
}
\details{
    Data set printing is like \code{data.frame}, but with different
    justification for the columns, styling, and truncated output.
}
\seealso{
\code{\link{print.data.frame}}
}
\examples{
# truncates at 20 rows by default
print(as_dataset(mtcars))

# change the default to 5:
rows <- options(frame.rows = 5)[[1]]
print(as_dataset(mtcars))
options(frame.rows = rows) # restore old value

# print all rows:
print(as_dataset(mtcars), -1)

# truncates before wrapping lines
x <- cbind(mtcars, mtcars, mtcars)
print(as_dataset(x))

# print all columns:
print(as_dataset(x), wrap = -1)
}
