\name{reorder.dataset}
\alias{reorder.dataset}
\title{Data Reordering}
\description{
Reorder the rows in a dataset.
}
\usage{
\S3method{reorder}{dataset}(x, ..., enclos = parent.frame())
}
\arguments{
\item{x}{a dataset object.}

\item{...}{a sequence of expressions that evaluate to numeric, complex,
    character, or logical vectors with lengths equal to the number of
    rows in \code{x}.}

\item{enclos}{an enclosing environment for variables in the expressions that
    don't correspond to columns in \code{x}.}
}
\details{
\code{reorder} reorders the rows of \code{x} according to the specified
expressions. These expressions get evaluated in a context that looks in the
columns of \code{x} first, then in the \code{encos} environment.
After evaluating the arguments, we determine the order according to
\code{order(, method = "radix")}, a stable sorting method that does not
depend on the current character locale.

By default, we sort in ascending order according to the arguments, in order.
To use descending order for one or more of the arguments use \code{desc =}
before the argument; see the examples below. You can also specific
\code{asc =}, but this has no effect as ascending order is the default.
}
\value{
A \code{dataset} with the rows of \code{x} put in the specified order.
}
\seealso{
\code{\link{order}}.
}
\examples{
x <- as_dataset(mtcars)

# order by 'cyl'
reorder(x, cyl)

# break ties using 'gear'
reorder(x, cyl, gear)

# order by 'cyl' descending, 'gear' ascending
reorder(x, desc = cyl, gear)

# order by 'cyl' descending, 'gear' descending
reorder(x, desc = cyl, desc = gear)
}
