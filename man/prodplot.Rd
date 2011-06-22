\name{prodplot}
\alias{prodplot}
\title{Create a product plot...}
\usage{prodplot(data, formula, divider=mosaic(), cascade=0,
    scale_max=TRUE, na.rm=FALSE, levels=-1L, subset, ...)}

\description{
  Create a product plot
}
\arguments{
  \item{data}{input data}
  \item{formula}{pdf description formula.}
  \item{divider}{way to partition space}
  \item{cascade}{degree of cascading}
  \item{scale_max}{should lower levels be scaled to take up as much space as 
possible (TRUE), or keep the same ratio between area and value as the 
parent (FALSE).}
  \item{na.rm}{should missing levels be removed?}
}
\examples{prodplot(happy, ~ happy, "hbar")
prodplot(happy, ~ happy, "hspine")

prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
prodplot(happy, ~ sex + happy, stacked())

# The levels argument can be used to extract a given level of the plot
prodplot(happy, ~ sex + happy, stacked(), subset = .(level == 1))
prodplot(happy, ~ sex + happy, stacked(), subset = .(level == 2))}
