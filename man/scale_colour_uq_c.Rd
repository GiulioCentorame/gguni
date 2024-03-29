% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uq.R
\name{scale_colour_uq_c}
\alias{scale_colour_uq_c}
\title{Continuous ggplot2 palette for \code{colour}}
\usage{
scale_colour_uq_c(
  palette = c("main", "secondary", "bw", "discrete", "continuous", "neutral"),
  alpha = 1,
  reverse = FALSE,
  ...
)
}
\arguments{
\item{palette}{Palette name (one of \code{main}, \code{secondary}, \code{bw}, \code{discrete}, \code{continuous}, or \code{neutral}). Default: \code{main}}

\item{alpha}{Colour transparency. Default: 1 (no transparency)}

\item{reverse}{If \code{TRUE}, reverse the colour palette order. Default: \code{FALSE}}

\item{...}{Additional arguments to pass to \code{ggplot2::discrete_scale()}}
}
\value{
A continuous ggplot2 palette for \code{colour}
}
\description{
Continuous ggplot2 palette for \code{colour}
}
\examples{
example_plot <-
ggplot(diamonds, aes(carat, depth, colour = price)) +
 geom_point()

# normal palette
example_plot + scale_colour_uq_c(palette = "continuous")

# add transparency
example_plot + scale_colour_uq_c(palette = "continuous",
                                 alpha = 0.6)
# reverse palette order
example_plot + scale_colour_uq_c(palette = "continuous",
                                 reverse = TRUE)
}
