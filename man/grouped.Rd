\name{grouped}
\alias{grouped}
\alias{grouped.dataset}
\alias{grouped.default}
\title{Grouped Data and Computations}
\description{
Group the values in a data set according to a specified factor,
optionally performing a computation on each group.
}
\usage{
grouped(x, by = NULL, do = NULL, ...)
}
\arguments{
\item{x}{object to be grouped.}

\item{by}{a dataset of grouping factors for the rows, identifiers for the columns
    in \code{x} to group by, or \code{NULL}.}

\item{do}{function to apply to each group, or \code{NULL}.}

\item{\dots}{optional arguments to \code{do} function.}
}
\details{
\code{grouped} is a generic method that splits the rows in a data set
according to a grouping factor (the \code{by} argument), and then optionally
performs a computation on each group.  One can specify the grouping factor
using the \code{by} argument:
\itemize{
    \item if a character vector of column names in \code{x} or an integer
    vector of column indices in \code{x}, then these columns get removed
    from \code{x} in the converted object and used as grouping factor
    columns;

    \item if a matrix-like object, then this object is converted to a
    \code{dataset} and used as the grouping factor;

    \item if \code{NULL}, then the all rows in \code{x} get grouped together.
}
Aside from the behavior for \code{by = NULL}, the argument behaves similarly
to the \code{keys} argument to the \code{\link{framed}} function.
}
\value{
A dataset object with values equal to the result of the \code{do} function on
each group and keys equal to the unique grouping factor values. Or, if
\code{do = NULL}, then the values in the result are data sets, the groups
themselves.
}
\seealso{
\code{\link{framed}}, \code{\link{split}}, \code{\link{tapply}}.
}
\examples{
# split into groups
grouped(mtcars, c("gear", "cyl"))

# apply a function to each group
grouped(mtcars, c("gear", "cyl"), function(x)
        list(n = nrow(x), mpg = mean(x$mpg), hp = mean(x$hp)))
}
