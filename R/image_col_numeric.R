#' Colorize an Image according to Gray Scale
#' 
#' A color image can be converted to one with 
#' different degrees of gray. Then, colors in a 
#' palette can be added according to the gray 
#' degrees. The function is a simple wrapper 
#' of \code{scales::col_numeric}.
#'
#' @param x  an image
#' read into R by \code{magick::image_read}.
#' @param palette two or more colors. The default is 
#' c("purple", "yellow") which means the deeper colors
#' on the image will become purple and the lighter 
#' yellow.
#' @param n the max num of colors that will be used. 
#' The default is 256. Note, the number of colors that 
#' really exist may be smaller than this number.
#' @param alpha whether 
#' transparency is used. Transparency
#' only exists when \code{alpha = TRUE} and your 
#' image is in the format (e. g., png) that supports 
#' transparency. The default is FALSE.
#' @param result if it is "magick" (default), the result 
#' is a picture of the same type used by package magick.
#' If it is "raster", the result is a matrix that can be 
#' used as a raster by \code{ggplot2::annotation_raster}.
#' @param res resolution that is used by 
#' \code{magick::image_graph}. The default is 144.
#' 
#' @export
image_col_numeric=function(x, palette=c("purple", "yellow"), n=256, alpha=FALSE, result="magick", res=144){
	stopifnot(result %in% c("magick", "raster"))
	# palette must not have alpha
	palette=unlist(lapply(palette, FUN=function(x) if (grepl("#", x)) substr(x, start=1, stop=7) else x))
	cla=class(x)[1]
	if (! grepl("magick", cla)) stop("x must be a picture read into R by magick::image_read.")
	x=magick::image_quantize(x, max=n, colorspace="gray")
	width=magick::image_info(x)
	height=as.numeric(width[1, 3])
	width=as.numeric(width[1, 2])
	cha=as.character(grDevices::as.raster(x))
	pos=which(cha=="transparent")
	if (length(pos)>0) cha[pos]="#ffffff00"
	cha1=cha[1]
	alpha=if (alpha==TRUE & nchar(cha1)==9) TRUE else FALSE
	if (alpha==TRUE) alp=substr(cha, start=8, stop=9)
	cha=strtoi(substr(cha, start=2, stop=3), base=16)
	cha=scales::col_numeric(palette, domain=range(cha))(cha)
	if (alpha==TRUE){
		cha=paste0(cha, alp, sep="")
		if (length(pos)>0){
			cha[pos]="transparent"
		}
	}
	cha=matrix(cha, nrow=height, byrow=TRUE)
	if (result=="raster"){
		return(cha)
	} else {
		canv=magick::image_graph(width=width, height=height, bg="transparent", res=res)
		grid::grid.raster(image=cha, width=1, height=1)
		grDevices::dev.off()
		return(canv)
	}
}
