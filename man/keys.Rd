\name{keys}
\alias{keys}
\alias{keys.dataset}
\alias{keys.default}
\alias{keys<-}
\alias{keys<-.dataset}
\title{Data Keys}
\description{
Get or set the keys associated with a set of values.
}
\usage{
keys(x)
keys(x) <- value
}
\arguments{
  \item{x}{an object.}

  \item{value}{keys for \code{x}, or \code{NULL}.}
}
\details{
A \code{\link{dataset}} object and other similar objects can have a
\code{keys} attribute identifying the data values in the object. This
attribute is a dataset with unique rows. The key columns can be any
of the following basic types: logical, integer, double, complex, or
character.

Use \code{keys(x)} to retrieve the keys attribute for the rows in \code{x}.
Use \code{keys(x)<- value} to set attribute, or \code{keys(x) <- NULL} to
drop the keys.
}
\value{
    \code{keys(x)} is a dataset with the same number of rows as \code{x}.
}
\seealso{
\code{\link{lookup}}, \code{\link{keylevels}}, \code{\link{framed}}.
}
\examples{
# data.frame objects do not have keys
keys(mtcars)

# when converting a data.frame, keys are taken from the row names (if set)
x <- framed(mtcars)
keys(x)

# 'keys' can be scalar...
keys(x) <- runif(32)

# ...or 'keys' can have multiple columns
keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8),
                   k2 = rep(1:8, 4))

# set 'keys' to NULL to remove
keys(x) <- NULL
}
