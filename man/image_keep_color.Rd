% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_keep_color.R
\name{image_keep_color}
\alias{image_keep_color}
\title{Keep Some Colors Unchanged and Make 
Others into Grayscale}
\usage{
image_keep_color(x, color = NULL, fuzz = 10, result = "magick")
}
\arguments{
\item{x}{an image read into R 
by \code{magick::image_read}.}

\item{color}{the same 
as \code{magick::image_transparent}.
You can use 1 or more colors.}

\item{fuzz}{the same 
as \code{magick::image_transparent}. 
However, Its length must either be 1 or the same 
as \code{color}.}

\item{result}{if it is "magick" (default), the result is 
a magick image, if it is "raster", the result is a matrix.}
}
\description{
This function keeps pixels with certain 
colors unchanged and transforms 
others into grayscale.
The function is in fact a wrapper of 
\code{magick::image_transparent}, so it 
uses the latter's \code{color} and \code{fuzz}
parameters. NOTE: the function only works 
for fully opaque or fully 
transparent (labelled as "transparent") pixels.
}
