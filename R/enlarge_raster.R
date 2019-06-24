#' Enlarge a Color Matrix
#' 
#' This is a convenient wrapper of 
#' \code{colorRampPalette} to enlarge 
#' a color matrix or raster.
#' 
#' @param x a color matrix or raster. It should
#' have at least 1 row and 1 column with no NAs.
#' @param n a vector with 2 numbers. If it has 1 number, 
#' the number will be repeated twice. The two numbers 
#' indicate how many colors you will get in the result per 
#' row and per column. Default is \code{c(10, 10)}.
#' 
#' @export
#' @examples
#' library(ggplot2)
#' # the original matrix
#' m=matrix(c(
#' 	"red", "yellow", "green", 
#' 	"blue", "purple", "cyan", 
#' 	"black", "orange", "grey"), byrow=TRUE, nrow=3)
#' # enlarge the matrix
#' mm=enlarge_raster(m, c(15, 15))
#' ggplot()+xlim(0, 10)+ylim(0, 5)+coord_fixed()+
#' 	annotation_raster(mm, 
#' 		xmin=0, xmax=10, ymin=0, ymax=5, interpolate=TRUE)
enlarge_raster=function(x, n=c(10, 10)){
	cla=class(x)[1]
	stopifnot(cla %in% c("matrix", "raster"))
	if (cla == "raster") x=as.matrix(x)
	if (any(dim(x) < 1)) stop("x must have at least 1 row and 1 column.")
	if (anyNA(x)) stop("x must have no NAs.")
	if (length(n) == 1) n=rep(n, 2)
	y=t(apply(x, 1, ENLARGE_COLOR_VECTOR, N=n[1]))
	y=apply(y, 2, ENLARGE_COLOR_VECTOR, N=n[2])
	y
}

ENLARGE_COLOR_VECTOR=function(X, N=10) if (length(X) < N) grDevices::colorRampPalette(X)(N) else X	
