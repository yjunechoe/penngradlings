% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot2-colors.R
\name{pgl_pals}
\alias{pgl_pals}
\title{Generate a PGL-themed color palette}
\usage{
pgl_pals(what = NULL, who = NULL)
}
\arguments{
\item{what}{The object the palette was extracted from. It's sufficient to just supply this argument.}

\item{who}{The person associated with the palette. This argument is optional.}
}
\value{
A unary palette generating function that takes the number of colors to return.
}
\description{
Generate a PGL-themed color palette
}
\examples{
library(colorspace)
swatchplot(pgl_pals("blueberry_matcha_boba", "christine")())
swatchplot(pgl_pals("cat_coaster", "xin")())
swatchplot(pgl_pals("cat_coaster", "xin")(15))
swatchplot(pgl_pals("hk_postcard_dull", "may")())
swatchplot(pgl_pals("hk_postcard_sharp", "may")())
}
