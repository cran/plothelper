% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_circle_cm.R
\name{geom_circle_cm}
\alias{geom_circle_cm}
\title{Geom Layer for Circle with Absolute Size}
\usage{
geom_circle_cm(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  linetype = NULL,
  ...
)
}
\arguments{
\item{mapping}{aes mapping.}

\item{data}{data.}

\item{stat}{stat.}

\item{position}{position.}

\item{na.rm}{logical, whether to remove NA values.}

\item{show.legend}{whether to show legend.}

\item{inherit.aes}{logical, whether to inherit aes from ggplot().}

\item{linetype}{should always be NULL.
because it will not be used.}

\item{...}{additional parameters.}
}
\description{
This layer uses centimeter as unit to draw circles so 
that the size and shape will not be influenced by 
the change of the coordinate systems 
(even when a polar system is used). 
Note: this function does not have 
\code{linetype} and \code{n} arguments.
}
\details{
Accepted properties are: 
\itemize{
  \item (1) \code{rcm} radius in centimeter.
  \item (2) \code{color} color of the outline.
  \item (3) \code{fill} color inside the shape.
  \item (4) \code{alpha} alpha of color and fill.
  \item (5) \code{size} line width of the outline.
  \item (6) \code{x} x coordinates of the middle points. 
  \item (7) \code{y} y coordinates of the middle points.
}
}
\examples{
library(ggplot2)
dat=data.frame(x=1: 10, y=rep(5, 10), R=rep(c(0.5, 1), 5))
ggplot(dat)+xlim(0, 11)+ylim(1, 9)+
	geom_circle_cm(aes(x=x, y=y, fill=factor(R)), rcm=dat$R, alpha=0.5)
}
