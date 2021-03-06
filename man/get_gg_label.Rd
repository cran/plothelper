% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gg_label.R
\name{get_gg_label}
\alias{get_gg_label}
\title{Checking Min, Max, Labels and Label Positions}
\usage{
get_gg_label(
  a = NULL,
  b = NULL,
  v = NULL,
  gg = NULL,
  mult = 0.05,
  add = 0,
  axis = "y"
)
}
\arguments{
\item{a}{extreme values of a numeric vector. Note: only 
one of \code{a}, \code{v}, \code{gg} can be non-NULL.
It can also be a gg object.}

\item{b}{another extreme value if \code{a} is not NULL.}

\item{v}{a numeric vector.}

\item{gg}{a gg object created by ggplot function.
Which value will be checked depends on \code{axis}.}

\item{mult}{default is 0.05 and should be of length 
1 or 2. It mimics the \code{mult} argument of 
\code{ggplot2::expansion}. It is only used when 
a is numeric or v is non-NULL.}

\item{add}{default is 0. It mimics the \code{add} 
argument of \code{ggplot2::expansion}.}

\item{axis}{if \code{gg} is used or \code{a} is a 
ggplot object, 
which axis will 
be checked? It can be "x" or "y" (default).}
}
\description{
Given a numeric vector or a ggplot object, the function
will check the range, labels and label 
positions (the same as major grid lines) that 
will used on the axis. The result is a length 5 list for 
min limit, max limit, labels, major grid-line positions, 
all (major and minor) grid-line positions.
}
\examples{
get_gg_label(a=1, b=1000)
# The following three have the same results.
get_gg_label(a=1, b=1000, mult=0)
get_gg_label(v=c(1, 500, 1000), mult=0)
p=ggplot()+geom_point(aes(1: 3, c(1, 500, 1000)))+
  scale_y_continuous(expand=expansion(mult=0))
get_gg_label(gg=p)
}
