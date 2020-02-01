#' Modify R, G, B Values of an Image
#' 
#' 
#' The function modifies the R, G, B values 
#' of an image and is used in the same 
#' way as \code{image_modify_hsv} 
#' in this package. The 
#' three channels can be modified separately.
#' The ways to modify include: setting values 
#' to some specified values (set_*), adding (add_*), 
#' multiplying the original values (mult_*), 
#' rescaling the original values (rescale_*), 
#' using a function to recompute values (fun_*). 
#' The most useful way is to use some internal 
#' curves that mimic those PS-like apps.
#' 
#' @details
#'  Several internal curves can be used. 
#' Please see the Details part 
#' of \code{\link{image_modify_hsv}}.
#' 
#' @param x an image created 
#' by \code{magick::image_read} or 
#' other functions in package magick. 
#' @param set_r set r values with specific values.
#' @param add_r add specific 
#' values to current R values.
#' @param mult_r multiply the current values 
#' with specific values.
#' @param rescale_r a length 2 numeric vector 
#' specifying the desired range of R values, 
#' e. g., \code{rescale_r = c(180, 240)} which 
#' will make the smallest original value to 
#' be 180, and the largest, 240. Alternatively, 
#' it can be your own scaling function.
#' @param fun_r your own modifying function 
#' (e. g., \code{fun_r = sqrt}). Alternatively, it can 
#' be a list that designates how to use internal 
#' curves. See \code{\link{image_modify_hsv}}.
#' @param set_g,add_g,mult_g,rescale_g,fun_g parameters 
#' to change G values. Used in the same way as those 
#' for R. See above. 
#' @param set_b,add_b,mult_b,rescale_b,fun_b parameters 
#' to change B values. Used in the same way as those 
#' for R. See above.
#' @param alpha whether to allow 
#' the output colors have transparency. Default is FALSE.
#' @param result the default is "magick", the output is 
#' a magick picture. When it is "raster", a matrix is created 
#' which can be use as a raster 
#' for \code{ggplot2::annotation_raster}.
#' @param res when the result is a magick picture, the 
#' \code{res} parameter used by \code{magick::image_graph}.
#' Default is 144.
#' @param checks when modifications are done, whether 
#' to check the output values. The default is TRUE which 
#' means whether the computed R, G and B values are 
#' in the [0, 255] range will be checked, and, those > 255 will 
#' be set to 255 and those < 0 will be set to 0 automatically. 
#' However, you can turn off these checks and (FALSE).
#' @param warn when \code{checks = TRUE}, whether 
#' to create a warning when values > 255 or < 0 are found 
#' and coerced to [0, 255].
#' 
#' @export
image_modify_rgb=function(x, 
	set_r=NULL, add_r=NULL, mult_r=NULL, rescale_r=NULL, fun_r=NULL, 
	set_g=NULL, add_g=NULL, mult_g=NULL, rescale_g=NULL, fun_g=NULL, 
	set_b=NULL, add_b=NULL, mult_b=NULL, rescale_b=NULL, fun_b=NULL, 	
	alpha=FALSE, result="magick", res=144, checks=TRUE, warn=FALSE){
	
	stopifnot(result %in% c("magick", "raster"))
	it_is_pic=FALSE
	# In fact x can be a color vector. But this is not encouraged, so the manual does not mention it.
	if (grepl("magick", class(x)[1])){
		it_is_pic=TRUE
		x=as.raster(x)
		nrpic=nrow(x)
		ncpic=ncol(x)
		x=as.character(x)
	}
	
	y=grDevices::col2rgb(x, alpha=alpha)
	nc=ncol(y)
	
	## change r
	if ( (is.null(set_r))+(is.null(add_r))+(is.null(mult_r))+(is.null(rescale_r))+(is.null(fun_r)) != 5){
		rr=y[1, ]
		
		print_range=stats::quantile(rr, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original r are:\n")
		print(print_range)
		
		if (!is.null(set_r)) rr=rep_len(set_r, nc)
		if (!is.null(add_r)) rr=rr+add_r
		if (!is.null(mult_r)) rr=rr*mult_r
		if (!is.null(rescale_r)) rr=RESCALE_FUN_VEC(rr, para=rescale_r)
		if (!is.null(fun_r)){
			if (is.function(fun_r)){
				rr=match.fun(fun_r)(rr) 
			} else if (is.list(fun_r)){
				rr=scales::rescale(c(0, 255, rr), to=c(0, 1))[-c(1, 2)]
				rr=USE_INTERNAL_CURVE(rr, LIST=fun_r, cat_text=NULL)
				rr=scales::rescale(c(0, 1, rr), to=c(0, 255))[-c(1, 2)]
			}
		}
		
		rr=as.integer(rr)
		if (checks==TRUE){
			which_r_big=which(rr>255)
			which_r_small=which(rr<0)
			if (length(which_r_big)>0){
				rr[which_r_big]=255
				if (warn==TRUE) warning("Some adjusted r values are larger than 255 and are set to 255.")
			}
			if (length(which_r_small)>0){
				rr[which_r_small]=0
				if (warn==TRUE) warning("Some adjusted r values are smaller than 0 and are set to 0.")
			}
		}
		
		y[1, ]=rr
		rr=NULL
	}

	## change g	
	if ( (is.null(set_g))+(is.null(add_g))+(is.null(mult_g))+(is.null(rescale_g))+(is.null(fun_g)) != 5){
		gg=y[2, ]
		
		print_range=stats::quantile(gg, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original g are:\n")
		print(print_range)
		
		if (!is.null(set_g)) gg=rep_len(set_g, nc)
		if (!is.null(add_g)) gg=gg+add_g
		if (!is.null(mult_g)) gg=gg*mult_g
		if (!is.null(rescale_g)) gg=RESCALE_FUN_VEC(gg, para=rescale_g)
		if (!is.null(fun_g)){
			if (is.function(fun_g)){
				gg=match.fun(fun_g)(gg) 
			} else if (is.list(fun_g)){
				gg=scales::rescale(c(0, 255, gg), to=c(0, 1))[-c(1, 2)]
				gg=USE_INTERNAL_CURVE(gg, LIST=fun_g, cat_text=NULL)
				gg=scales::rescale(c(0, 1, gg), to=c(0, 255))[-c(1, 2)]
			}
		}
		
		gg=as.integer(gg)
		if (checks==TRUE){
			which_g_big=which(gg>255)
			which_g_small=which(gg<0)
			if (length(which_g_big)>0){
				gg[which_g_big]=255
				if (warn==TRUE) warning("Some adjusted g values are larger than 255 and are set to 255.")
			}
			if (length(which_g_small)>0){
				gg[which_g_small]=0
				if (warn==TRUE) warning("Some adjusted g values are smaller than 0 and are set to 0.")
			}
		}
		
		y[2, ]=gg
		gg=NULL
	}

	## change b	
	if ( (is.null(set_b))+(is.null(add_b))+(is.null(mult_b))+(is.null(rescale_b))+(is.null(fun_b)) != 5){
		bb=y[3, ]
		
		print_range=stats::quantile(bb, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original b are:\n")
		print(print_range)		
		
		if (!is.null(set_b)) bb=rep_len(set_b, nc)
		if (!is.null(add_b)) bb=bb+add_b
		if (!is.null(mult_b)) bb=bb*mult_b
		if (!is.null(rescale_b)) bb=RESCALE_FUN_VEC(bb, para=rescale_b)
		if (!is.null(fun_b)){
			if (is.function(fun_b)){
				bb=match.fun(fun_b)(bb) 
			} else if (is.list(fun_b)){
				bb=scales::rescale(c(0, 255, bb), to=c(0, 1))[-c(1, 2)]
				bb=USE_INTERNAL_CURVE(bb, LIST=fun_b, cat_text=NULL)
				bb=scales::rescale(c(0, 1, bb), to=c(0, 255))[-c(1, 2)]
			}
		}
		
		bb=as.integer(bb)
		if (checks==TRUE){
			which_b_big=which(bb>255)
			which_b_small=which(bb<0)
			if (length(which_b_big)>0){
				bb[which_b_big]=255
				if (warn==TRUE) warning("Some adjusted b values are larger than 255 and are set to 255.")
			}
			if (length(which_b_small)>0){
				bb[which_b_small]=0
				if (warn==TRUE) warning("Some adjusted b values are smaller than 0 and are set to 0.")
			}
		}
		
		y[3, ]=bb
		bb=NULL
	}
	
	if (alpha==FALSE){
		y=apply(y, 2, FUN=function(xx) grDevices::rgb(xx[1], xx[2], xx[3], maxColorValue=255))
	} else {
		y=apply(y, 2, FUN=function(xx) grDevices::rgb(xx[1], xx[2], xx[3], alpha=xx[4], maxColorValue=255))
	}

	if ( (is.null(fun_r)) + (is.null(fun_g)) + (is.null(fun_b)) != 3) cat("Attention: when using internal curves, the function believes you have scaled r, g or b values into [0, 255] and does not check this.\n")
	
	if (it_is_pic == FALSE){
		return(y)
	} else {
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
}
