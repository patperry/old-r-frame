\name{as.list.dataset}
\alias{as.data.frame.dataset}
\alias{as.list.dataset}
\title{Data Set Conversion}
\description{
Convert a data set to a list or data frame.
}
\usage{
\method{as.data.frame}{dataset}(x, row.names = NULL, ..., stringsAsFactors = FALSE)
\method{as.list}{dataset}(x, ..., flat = FALSE, path = FALSE)
}
\arguments{
  \item{x}{a data set object.}

  \item{row.names}{row names for the converted object.}

  \item{stringsAsFactors}{whether to convert string columns to factors.}

  \item{flat}{whether to recursively convert matrix columns to lists
      of vectors.}

  \item{path}{whether to attach a \code{path} attribute to the result
      containing the paths to the columns in the original object.}

  \item{...}{further arguments passed to or from other methods.}
}
\details{
These functions convert a \code{\link{dataset}} object to a \code{data.frame}
or a list of columns. For \code{as.list}, if \code{flat = TRUE}, then any
matrix-like columns get converted to lists of vectors. For \code{as.data.frame},
matrix-like columns always get converted.
}
\value{
A \code{data.frame} or \code{list} with columns taken from \code{x}.

For \code{as.list}, if \code{path = TRUE}, then the result has two attributes:
named \code{"index"} and \code{"path"} giving the paths to the columns in the
original dataset. Since data set objects do not necessarily have unique column
names, it is possible for two columns in the result of
\code{as.list(x, flat = TRUE)} to have the same path; use the \code{"index"}
attribute to unambiguously identify the source of each column.
}
\seealso{
\code{\link{dataset}}.
}
\examples{
x <- dataset(a = matrix(1:6, 3, 2), b = runif(3))
y <- dataset(x, c = rnorm(3), d = rnorm(3))

# convert a data set with a matrix-like column
as.list(x)
as.list(x, flat = TRUE)
as.data.frame(x)

# convert a data set with a nested data set
as.list(y)
as.list(y, flat = TRUE)
as.data.frame(y)

# specify path = TRUE to get the paths in the original objects:
l <- as.list(y, flat = TRUE, path = TRUE)
attr(l, "path")
attr(l, "index")
}
