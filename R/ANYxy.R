#' Generating Groups of Coordinates for Any Polygon
#' 
#' Given your function to create a multiple of
#' points (for example, points to form a polygon), this 
#' function generates x and y coordinates for 
#' groups of points of the same type with different parameters.
#' The output of this function can be shown by 
#' \code{ellipsexy} and \code{rectxy} in this package.
#' 
#' @param myfun your function to generate a single polygon.
#' Note: \bold{the value of each argument of your function 
#' must be a single-value vector. And the result of your 
#' function should be a data frame!}. See examples.
#' @param ... named parameters used by your function. These 
#' parameters will be passed to \code{mapply}.
#' @param MoreArgs this will be passed to the \code{MoreArgs} 
#' argument of \code{mapply}.
#' @param group default is TRUE which means a column 
#' named "g" will be added to each data frame. This facilitates 
#' further drawing using \code{aes(..., group = g)}.
#' @param todf default is TRUE which means to combine the 
#' result into a data frame. Otherwise, the result is a list.
#' 
#' @export
#' @examples
#' library(ggplot2)
#' # First, you need a function to generate
#' # x and y coordinates for a single group
#' # of points.
#' x_square=function(start, end, A, B){
#' 	x=seq(start, end, 0.1)
#' 	data.frame(x=x, y=A*(x^2)+B)
#' }
#' # All the arguments of your function 
#' # (here, start, end, A, B) should only accept
#' # vectors of length 1. And, the result of 
#' # your function should be a data frame
#' # of x and y coordinates 
#' # (here, coordinates of curves).
#' dat=ANYxy(myfun=x_square, 
#'		start=-1, end=1, A=c(1, 2), MoreArgs=list(B=1), 
#'		group=TRUE, todf=TRUE)
#' ggplot(dat)+geom_line(aes(x, y, group=g, color=factor(g)))
ANYxy=function(myfun, ..., MoreArgs=NULL, group=TRUE, todf=TRUE){ 
	stopifnot(group %in% c(TRUE, FALSE))
	stopifnot(todf %in% c(TRUE, FALSE))
	stopifnot(is.function(myfun))
	if (! is.null(MoreArgs)) stopifnot(is.list(MoreArgs))
	FINAL=mapply(FUN=myfun, ..., MoreArgs=MoreArgs, SIMPLIFY=FALSE)
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}	
	if (todf) do.call(rbind, FINAL) else FINAL
}
