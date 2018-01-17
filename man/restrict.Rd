\name{restrict}
\alias{restrict}
\alias{restrict_slice}
\alias{restrict_slice.default}
\title{Slicing by Key}
\description{
Restrict a data set to the row keys matching a specified slice.
}
\usage{
restrict(...)

restrict_slice(x, slice, ..., drop = FALSE)
}
\arguments{
\item{...}{for \code{restrict}, the data set and the slice components;
    for \code{restrict_slice} further arguments past to or from other methods.}

\item{x}{an object with a key for each row.}

\item{slice}{the slice description, giving values for each key component.}

\item{drop}{logical indicating whether to drop key components where the
    slice is a singleton set.}
}
\details{
Extract a subset of \code{x} by getting the slice of rows with key
components belonging to the specified sets.

(fill in)
}
%\value{
%}
\seealso{
\code{\link{keyrows}}, \code{\link{rowid}}.
}
\examples{
# data in wide format
value <- matrix(1:12, 4, 3, dimnames = list(c("a", "b", "c", "d"),
                                            c("x", "y", "z")))
# same data, in narrow format
x <- dataset(value = as.vector(value))
keys(x) <- list(row = rownames(value)[row(value)],
                col = colnames(value)[col(value)])
print(x)

# restrict to rows with keys in the specified sets, dropping singleton keys
restrict(x, row = c("b", "d"))
restrict(x, row = c("b", "d"), col = "y")
restrict(x, col = "x")

# use I() to prevent dropping
restrict(x, col = I("x"))

# alternatively, slice by indexing with a list; here default behavior is
# not to drop; switch to `restrict` defaults by setting `drop = TRUE`
restrict_slice(x, list(c("b", "d"), "y"))
restrict_slice(x, list(c("b", "d"), "y"), drop = TRUE)
}
