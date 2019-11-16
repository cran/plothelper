#' Keep Several Colors Unchanged 
#' and Make Other Parts Gray
#' 
#' Suppose you have a picture with a 
#' red flower on it. This function keeps 
#' the red color unchanged and make 
#' other parts of the picture into 
#' black-gray-white.
#'
#' @param x an image created by 
#' \code{magick::image_read} or 
#' other functions in package magick.  
#' @param color a vector of one or more colors that will 
#' be kept unchanged.
#' @param fuzz a number between 0 and 100 (default is 
#' 10). Suppose you want the red color to be kept
#' unchanged and set fuzz to 0, then what remain is 
#' only red. However, if you set it to 10, colors similar 
#' to red will also be kept unchanged.
#' @param alpha whether allow the output image 
#' has transparency. Default is FALSE.
#' @param weight a three number vector deciding 
#' the weight of red, green and blue in 
#' computing gray color. Default is 
#' \code{c(0.299, 0.587, 0.114)}, so for example, 
#' red becomes 76, 76, 76 (which is the sum of 
#' 255 * 0.299 + 0 * 0.587 + 0 * 0.114).
#' @param result when it is "magick", the output 
#' is a magick image, "raster" for a matrix that can 
#' be a raster used for \code{ggplot2::annotation_raster}.
#' @param res the \code{res} parameter used by 
#' \code{magick::image_graph} when the result is 
#' a magick image.
#' 
#' @export
image_keep_color=function(x, color=NULL, fuzz=10, alpha=FALSE, weight=c(0.299, 0.587, 0.114), result="magick", res=144){
	max_dist=7.65*fuzz # (255+255+255)*fuzz/100, simple dist, not Euclidean, for speed
	stopifnot(sum(weight) <= 1)
	stopifnot(result %in% c("magick", "raster"))
	if ( ! grepl("magick", class(x)[1])) stop("x must be a picture read into R by magick::image_read.")
	if (length(color)==0) stop("At least one color must be given.")
	if (identical(color, "click")) color=get_click_color(x)
	color=grDevices::col2rgb(color, alpha=FALSE)[1: 3, , drop=FALSE]
	ncolor=ncol(color)
	pic_info=as.numeric(magick::image_info(x)[1, 2: 3])
	pic_width=pic_info[1]
	pic_height=pic_info[2]
	x=as.character(as.raster(x))
	x=grDevices::col2rgb(x, alpha=alpha)
	if (alpha==TRUE) alp_store=x[4, ]
	x=base::t(x[1: 3, , drop=FALSE])
	
	judging=rep(1, nrow(x))
	for (i in 1: ncolor){
		ii=color[, i]
		judge_this=CHECK_COLOR_DIST(DF=x, R=ii[1], G=ii[2], B=ii[3], MAX_DIST=max_dist)
		judging=judging*judge_this
	}
	
	x=t(apply(cbind(x, judging), 1,  FUN=BECOME_GRAY, RP=weight[1], GP=weight[2], BP=weight[3]))
		
	x=if (alpha==FALSE) grDevices::rgb(r=trunc(x), maxColorValue=255) else grDevices::rgb(r=trunc(x), maxColorValue=255, alpha=alp_store)

	x=matrix(x, byrow=TRUE, nrow=pic_height)
	if (result=="raster"){
		return(x)
	} else {
		canv=magick::image_graph(width=pic_width, height=pic_height, bg="transparent", res=res)
		grid::grid.raster(image=x, width=1, height=1)
		grDevices::dev.off()
		return(canv)
	}
}

BECOME_GRAY=function(X, RP=0.299, GP=0.587, BP=0.114) if (X[4]==1) rep(sum(c(X[1]*RP, X[2]*GP, X[3]*BP)), 3) else X[-4]

CHECK_COLOR_DIST=function(DF, R, G, B, MAX_DIST)  ifelse ((abs(DF[, 1]-R)+abs(DF[, 2]-G)+abs(DF[, 3]-B)) < MAX_DIST, 0, 1)
