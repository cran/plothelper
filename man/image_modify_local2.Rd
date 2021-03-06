% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_modify_local2.R
\name{image_modify_local2}
\alias{image_modify_local2}
\title{Modify both a Subregion  and the Whole
of an Image}
\usage{
image_modify_local2(x, FUN1, FUN2 = NULL, geometry = "click", rectangle = TRUE)
}
\arguments{
\item{x}{an image read into R by 
\code{magick::image_read} or an image  
modified by functions in the magick 
package.}

\item{FUN1}{a function to modify 
a subregion of \code{x}. NOTE: the result 
of these functions must be of the same class 
as \code{x} and should not change the sizes 
of the subregion.}

\item{FUN2}{a function to modify 
the whole image, which must not change 
the size of the image. If it is NULL (default), 
nothing will do to the whole image.}

\item{geometry}{this parameter is 
different from the one used in package 
magick. Here, in this function, you can 
set \code{geometry = "click"} if you want 
to show which part is the subregion 
by mouse click (see 
function \code{image_crop_click} for how to 
use mouse click). Otherwise, you can use 
a length 4 vector with the exact order: left, 
right, top, bottom.}

\item{rectangle}{if it is TRUE (default), 
the subregion is a rectangle area. If 
it is FALSE, the subregion is 
an irregular polygon area, and, now 
\code{geometry} is ignored, you must 
designate the area by mouse click.}
}
\description{
The function is similar to 
\code{image_modify_local} but with different 
parameters. It modifies both a subregion  
of the image and the whole image, and then 
combines them. The subregion can be chosen 
either by numeric values or by mouse click ,which 
is the same as \code{image_modify_local}.
}
