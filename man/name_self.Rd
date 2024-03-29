% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaprogramming.R
\name{name_self}
\alias{name_self}
\title{Construct a list that names itself}
\usage{
name_self(list_expr, .fn = ~.x)
}
\arguments{
\item{list_expr}{A expression using a call to \code{list}, \code{c}, or the like, where all arguments
are name-value pairs and the expression evaluates to a list.}

\item{.fn}{A function passed to \code{purrr::imap_chr}, called on the character vector returned
after coercing the element symbols in \code{list_expr} to name. Defaults to the identity function.}
}
\value{
A named list
}
\description{
Construct a list that names itself
}
\note{
Names of named elements are preserved.
}
\examples{
name_self(c(min, mean, max))
name_self(c(min, mean, max), .fn = ~ toupper(.x))
name_self(c(min, mean, max), .fn = function(x, y) {
  paste0(x, y)
})
name_self(c(min, mean, custom_name = max))

# Helps when naming produced by from `across()`
suppressPackageStartupMessages(library(dplyr))

## With `name_self()`, columns are named after the functions
mtcars \%>\%
  group_by(cyl) \%>\%
  summarize(across(where(~ max(.x) > 100), name_self(list(min, mean, max))))

## More specifically, it allows `"{.fn"}` inside `.names` to reference the function names
mtcars \%>\%
  group_by(cyl) \%>\%
  summarize(across(disp, name_self(list(min, avg = mean, max)), .names = "{.col}.{toupper(.fn)}"))

## Without `name_self()`, column names are suffixed with position indices
mtcars \%>\%
  group_by(cyl) \%>\%
  summarize(across(where(~ max(.x) > 100), list(min, mean, max)))
}
