\name{order_by}
\alias{order_by}
\alias{order_by.default}
\alias{order_by.dataset}
\title{Data Ordering}
\description{
Reorder the rows in a dataset.
}
\usage{
order_by(x, ..., enclos = parent.frame())
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
\code{order_by} reorders the rows of \code{x} according to the specified
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
# order by 'cyl'
order_by(mtcars, cyl)

# break ties using 'gear'
order_by(mtcars, cyl, gear)

# order by 'cyl' descending, 'gear' ascending
order_by(mtcars, desc = cyl, gear)

# order by 'cyl' descending, 'gear' descending
order_by(mtcars, desc = cyl, desc = gear)
}
