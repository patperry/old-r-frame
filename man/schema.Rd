\name{schema}
\alias{schema}
\alias{schema.dataset}
\alias{schema.default}
\title{Data Schema}
\description{
Get type information for the columns in a data set.
}
\usage{
schema(x, ...)
}
\arguments{
\item{x}{a data set.}

\item{...}{further arguments passed to or from other methods.}
}
\details{
\code{schema(x)} returns a data set giving type information for the columns in
\code{x}. This is a generic method. The default implementation calls
\code{as_dataset} on its argument and reports the schema of the result.
}
\value{
A dataset with rows corresponding to the columns in \code{x}. The result has
column named \code{"class"}
giving the first component of the class of each column of \code{x}. When
\code{names(x)} is non-\code{NULL}, the result will also have a column called
\code{"name"} and give the column names. The keys for the result will be a
keyset with a single column, \code{"id"} giving the index of each column in
\code{x}.
}
\seealso{
\code{\link{print.dataset}}.
}
\examples{
# type information
schema(iris)

# data display
print(as_dataset(iris))
}
