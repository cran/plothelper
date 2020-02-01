#' Modify R, G, B Values according to 
#' V values
#' 
#' While the \code{\link{image_modify_rgb}} 
#' function modifies R, G, B with reference to 
#' the original values, 
#' \code{image_modify_rgb_v} also takes into 
#' account the brightness (V) values. It is similar 
#' to those apps which divide an image into 
#' a bright part and a dark part (and, for 
#' example, you can increase red in the 
#' bright part and decrease red in the 
#' dark part.
#' 
#' @details
#' This function uses custom functions 
#' or internal curves to 
#' make modification. See the Details part 
#' of \code{\link{image_modify_hsv}} to 
#' know how to use them. Note: values will 
#' be coerced to be in the [0, 255] range with 
#' no warning. For example, the original value 
#' is 240 and it becomes 280 in the output, then 
#' it will be set to 255 automatically.
#' 
#' @param x an image created 
#' by \code{magick::image_read} or 
#' other functions in package magick. 
#' @param fun_r,fun_g,fun_b a function or a list which 
#' designates an internal curve. See the Details part of 
#' \code{\link{image_modify_hsv}}.
#' @param alpha whether to allow 
#' the output colors have transparency. Default is FALSE.
#' @param rescale_v You can rescale the V values before 
#' modifying colors. A desired range of V values can 
#' be given, 
#' e. g., \code{rescale_v = c(0.2, 1)} which 
#' will make the smallest original value to 
#' be 0.2, and the largest, 1. Alternatively, 
#' it can be your own scaling function.
#' @param result the default is "magick", the output is 
#' a magick picture. When it is "raster", a matrix is created 
#' which can be use as a raster 
#' for \code{ggplot2::annotation_raster}.
#' @param res when the result is a magick picture, the 
#' \code{res} parameter used by \code{magick::image_graph}.
#' Default is 144.
#' 
#' @export
image_modify_rgb_v=function(x, fun_r=NULL, fun_g=NULL, fun_b=NULL, 	
	alpha=FALSE, rescale_v=NULL, result="magick", res=144){
	
	stopifnot(result %in% c("magick", "raster"))
	if ( ! grepl("magick", class(x)[1])) stop("x must be a picture read into R by magick::image_read.")
	x=as.raster(x)
	nrpic=nrow(x)
	ncpic=ncol(x)
	x=as.character(x)
	
	y=grDevices::col2rgb(x, alpha=alpha)
	vv=grDevices::rgb2hsv(y[1: 3, , drop=FALSE])[3, ]
	if (!is.null(rescale_v)) vv=RESCALE_FUN_VEC(vv, para=rescale_v)
	
	print_range=stats::quantile(vv, probs=c(0, 0.25, 0.5, 0.75, 1))
	cat("The original v are:\n")
	print(print_range)
	
	## change r
	if ( ! is.null(fun_r)){
		rr=y[1, ]
		
		print_range=stats::quantile(rr, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original r are:\n")
		print(print_range)

		if (is.function(fun_r)){
			rr=rr*(1+(match.fun(fun_r)(vv)-vv))
		} else if (is.list(fun_r)){
			rr=rr*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_r, cat_text=NULL)-vv))
		}
	
		rr=as.integer(rr)
		rr[rr > 255]=255
		rr[rr < 0]=0
		y[1, ]=rr
		rr=NULL
	}

	## change g	
	if ( ! is.null(fun_g)){
		gg=y[2, ]
		
		print_range=stats::quantile(gg, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original g are:\n")
		print(print_range)

		if (is.function(fun_g)){
			gg=gg*(1+(match.fun(fun_g)(vv)-vv))
		} else if (is.list(fun_g)){
			gg=gg*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_g, cat_text=NULL)-vv))
		}
	
		gg=as.integer(gg)
		gg[gg > 255]=255
		gg[gg < 0]=0
		y[2, ]=gg
		gg=NULL
	}

	## change b	
	if ( ! is.null(fun_b)){
		bb=y[3, ]
		
		print_range=stats::quantile(bb, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original b are:\n")
		print(print_range)

		if (is.function(fun_b)){
			bb=bb*(1+(match.fun(fun_b)(vv)-vv))
		} else if (is.list(fun_b)){
			bb=bb*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_b, cat_text=NULL)-vv))
		}
	
		bb=as.integer(bb)
		bb[bb > 255]=255
		bb[bb < 0]=0
		y[3, ]=bb
		bb=NULL
	}
	
	if (alpha==FALSE){
		y=apply(y, 2, FUN=function(xx) grDevices::rgb(xx[1], xx[2], xx[3], maxColorValue=255))
	} else {
		y=apply(y, 2, FUN=function(xx) grDevices::rgb(xx[1], xx[2], xx[3], alpha=xx[4], maxColorValue=255))
	}

	if ( (is.null(fun_r)) + (is.null(fun_g)) + (is.null(fun_b)) != 3) cat("Attention: when using internal curves, the function believes you have scaled r, g or b values into [0, 255] and does not check this.\n")
	
	y=matrix(y, nrow=nrpic, byrow=TRUE)
	if (result=="raster"){
		return(y)
	} else {
		canv=magick::image_graph(width=ncpic, height=nrpic, bg="transparent", res=res, clip=FALSE)
		grid::grid.raster(image=y, width=1, height=1)
		grDevices::dev.off()
		return(canv)
	}
}
