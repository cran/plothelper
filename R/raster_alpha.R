#' Combine a Matrix of Colors and 
#' a Matrix of Alpha Values
#' 
#' The function is a wrapper of 
#' \code{scales::alpha}. While the latter 
#' only works on vectors, the former 
#' can combine a matrix of colors and 
#' a matrix of alpha values as long as the two 
#' have the same numbers of rows and 
#' columns. 
#' 
#' @param color a matrix of colors, a 
#' raster or an image read into R by 
#' \code{magick::image_read}. 
#' @param alpha either a single value (e.g., 0.4)
#' or a matrix of alpha values. The matrix 
#' should have the same numbers of rows 
#' and columns as \code{color}.
#' 
#' @export
#' @examples
#' # A color matrix
#' co=c("red", "yellow", "green", "blue")
#' co=rbind(co, co, co)
#' # An alpha matrix
#' alp=c(1, 0.6, 0.3, 0.1)
#' alp=rbind(alp, alp, alp)
#' # Now combine the two
#' result=raster_alpha(co, alp)
raster_alpha=function(color, alpha){
	cla_color=class(color)[1]
	if (grepl("magick", cla_color)){
		color=as.matrix(grDevices::as.raster(color))
	} else if (cla_color=="raster"){
		color=as.matrix(color)
	}
	if (! is.matrix(color)) stop("color must be of class matrix, raster or magick-image.")
	rownames(color)=NULL # MUST ADD IF USING alply
	
	stopifnot(is.numeric(alpha))
	len=length(alpha)
	if (len==1){
		alpha=as.numeric(alpha)
		res=apply(color, 2, FUN=function(x) scales::alpha(x, alpha=alpha))
	} else {
		stopifnot(is.matrix(alpha))
		if ( ! identical(dim(color), dim(alpha))) stop("color and alpha must all be matrices and have the same numbers of rows and columns.")
		rownames(color)=NULL
		rownames(alpha)=NULL
		color=asplit(color, 2)
		alpha=asplit(alpha, 2)
		# color=plyr::alply(color, .margins=2, .fun=function(x) x)
		# alpha=plyr::alply(alpha, .margins=2, .fun=function(x) x)
		res=mapply(
			FUN=scales::alpha, 
			colour=color, 
			alpha=alpha, 
			SIMPLIFY=TRUE
		)
	}
	colnames(res)=NULL
	res
}
