#' Modify the H, S, V Values of a 
#' Color Vector or an Image
#' 
#' The function modifies the H, S, V values 
#' of a vector of colors or an image. The 
#' three channels can be modified separately.
#' However, the most frequently used is 
#' only the V modification.
#' The ways to modify include: setting values 
#' to some specified values (set_*), adding (add_*), 
#' multiplying the original values (mult_*), 
#' rescaling the original values (rescale_*), 
#' using a function to recompute values. 
#' The most useful way is to use some internal 
#' curves that mimic those PS-like apps.
#' DO see Details.
#' 
#' @details
#' \code{fun_*} can be a function or 
#' a named list which tells the 
#' function which internal function is to be used. 
#' You must ensure values used by the function 
#' specified by you to be in the range [0, 1] for 
#' H, S, V modification and [0, 255] for R, G, B
#' modification. Also, you'd better make sure 
#' the output values of the funtion are in 
# [0, 1] for H, S, V and [0, 255] for R, G, B.
#'  
#' When \code{fun_*} is a list, it should be written in the 
#' fowllowing way: 
#' \itemize{
#'   \item (1) \code{fun_* = list(fun = "s", c1 = -2, 
#' c2 = 2, domain = c(0, 1))} An "s" curve will be 
#' used. c1 points out how to deal with values below 
#' 0.5, c2 with values above 0.5. For c1 and c2, a value 
#' larger than 0 means a curvature towards y = 1, and 
#' a value smaller than 0 means a curvature towards
#' y = 0. So, c1 < 0 and c2 > 0 will make an s shape 
#' curve. c1 and c2 can be any number, though 
#' those with absolute values below 4 are quite 
#' good (default is -2 and 2). 0 means no change. 
#' domain specifies the 
#' value domain to put the result. The default is 
#' c(0, 1) which means not to rescale, thus
#' 0.1 is 0.1. However, if you 
#' set \code{domain = c(0.5, 1)}, 
#' then 0.1 will be 0.55. If you do not know how 
#' to set domain, just ignore it.
#'   \item (2) \code{fun_* = list(fun = "circle", 
#' value = 0.5)} When the fun 
#' is "circle" or "c", an arc will be used. value must 
#' be a number between -1 and 1 (default is 0.5). 
#' A number larger than 0 means the curvature is 
#' towards y = 1, and a number smaller than 0 means 
#' it is towards y = 0. value should not be 0.
#'   \item (3) \code{list(fun_* = "linear", x0 = 0.4, 
#' y0 = 0.6)} This makes a linear modification except 
#' that there is a breakpoint. The default point is 
#' (0.4, 0.6) which means: suppose all the original numbers
#' and output numbers are in the [0, 1] range and 
#' the points with their x position smaller than 0.4 will 
#' be put along the line that links (0, 0) and (0.4, 0.6), 
#' and, those with x position larger than 0.4 will be put 
#' along the line that links (0.4, 0.6) and (1, 1). 
#' }
#' 
#' @param x an image created by \code{image_read} or 
#' other functions in package magick. Alternatively, it 
#' can be a vector of colors.
#' @param set_h set H values with specific values.
#' @param add_h add specific 
#' values to current H values.
#' @param mult_h multiply the current values 
#' with specific values.
#' @param rescale_h a length 2 numeric vector 
#' specifying the desired range of H values, 
#' e. g., \code{rescale_h = c(0.6, 0.95)} which 
#' will make the smallest original value to 
#' be 0.6, and the largest, 0.95. Alternatively, 
#' it can be your own scaling function.
#' @param fun_h your own modifying function 
#' (e. g., \code{fun_h = sqrt}. Alternatively, it can 
#' be a list that designats how to use internal 
#' curves. See Details.
#' @param set_s,add_s,mult_s,rescale_s,fun_s parameters 
#' to change S values. Used in the same way as those 
#' for H. See above. 
#' @param set_v,add_v,mult_v,rescale_v,fun_v parameters 
#' to change V values. Used in the same way as those 
#' for H. See above.
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
#' means whether the computed H, S and V values are 
#' in the [0, 1] range will be checked, and, those > 1 will 
#' be set to 1 and those < 0 will be set to 0 automatically. 
#' However, you can turn off these checks and (FALSE).
#' @param warn when \code{checks = TRUE}, whether 
#' to create a warning when values > 1 or < 0 are found 
#' and coerced to [0, 1].
#' 
#' @export
#' @examples
#' \donttest{
#' # First create an image
#' library(magick)
#' mycolor=grDevices::hsv(0, s=seq(0.1, 0.9, 0.1), 
#' 	v=seq(0.1, 0.9, 0.1))
#' img=image_graph(width=400, height=400)
#' print(showcolor(mycolor)+theme_void())
#' dev.off()
#' # Now increase S values with 
#' # an internal circle curve and 
#' # set V values between [0.5, 1].
#' res=image_modify_hsv(img, 
#' 	fun_s=list("circle", value=1), 
#' 	rescale_v=c(0.5, 1))
#' }
image_modify_hsv=function(x, 
	set_h=NULL, add_h=NULL, mult_h=NULL, rescale_h=NULL, fun_h=NULL, 
	set_s=NULL, add_s=NULL, mult_s=NULL, rescale_s=NULL, fun_s=NULL, 
	set_v=NULL, add_v=NULL, mult_v=NULL, rescale_v=NULL, fun_v=NULL, 	
	alpha=FALSE, result="magick", res=144, checks=TRUE, warn=FALSE){
	
	stopifnot(result %in% c("magick", "raster"))
	it_is_pic=FALSE
	if (grepl("magick", class(x)[1])){
		it_is_pic=TRUE
		x=as.raster(x)
		nrpic=nrow(x)
		ncpic=ncol(x)
		x=as.character(x)
	}
	
	y=grDevices::col2rgb(x, alpha=alpha)
	if (alpha==TRUE) alp_store=y[4, ]/255
	y=grDevices::rgb2hsv(y[1: 3, , drop=FALSE])
	nc=ncol(y)
	
	## change h
	if ( (is.null(set_h))+(is.null(add_h))+(is.null(mult_h))+(is.null(rescale_h))+(is.null(fun_h)) != 5){
		hh=y[1, ]
		
		print_range=stats::quantile(hh, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original h are:\n")
		print(print_range)
		
		if (!is.null(set_h)) hh=rep_len(set_h, nc)
		if (!is.null(add_h)) hh=hh+add_h
		if (!is.null(mult_h)) hh=hh*mult_h
		if (!is.null(rescale_h)) hh=RESCALE_FUN_VEC(hh, para=rescale_h)
		if (!is.null(fun_h)) hh=if (is.function(fun_h)) match.fun(fun_h)(hh) 
			else if (is.list(fun_h)) USE_INTERNAL_CURVE(hh, LIST=fun_h, cat_text=NULL)
		
		if (checks==TRUE){
			which_h_big=which(hh>1)
			which_h_small=which(hh<0)
			if (length(which_h_big)>0){
				hh[which_h_big]=1
				if (warn==TRUE) warning("Some adjusted h values are larger than 1 and are set to 1.")
			}
			if (length(which_h_small)>0){
				hh[which_h_small]=0
				if (warn==TRUE) warning("Some adjusted h values are smaller than 0 and are set to 0.")
			}
		}
		
		y[1, ]=hh
		hh=NULL
	}

	## change s	
	if ( (is.null(set_s))+(is.null(add_s))+(is.null(mult_s))+(is.null(rescale_s))+(is.null(fun_s)) != 5){
		ss=y[2, ]
		
		print_range=stats::quantile(ss, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original s are:\n")
		print(print_range)
		
		if (!is.null(set_s)) ss=rep_len(set_s, nc)
		if (!is.null(add_s)) ss=ss+add_s
		if (!is.null(mult_s)) ss=ss*mult_s
		if (!is.null(rescale_s)) ss=RESCALE_FUN_VEC(ss, para=rescale_s)
		if (!is.null(fun_s)) ss=if (is.function(fun_s)) match.fun(fun_s)(ss) 
			else if (is.list(fun_s)) USE_INTERNAL_CURVE(ss, LIST=fun_s, cat_text=NULL)
		
		if (checks==TRUE){
			which_s_big=which(ss>1)
			which_s_small=which(ss<0)
			if (length(which_s_big)>0){
				ss[which_s_big]=1
				if (warn==TRUE) warning("Some adjusted s values are larger than 1 and are set to 1.")
			}
			if (length(which_s_small)>0){
				ss[which_s_small]=0
				if (warn==TRUE) warning("Some adjusted s values are smaller than 0 and are set to 0.")
			}
		}
		
		y[2, ]=ss
		ss=NULL
	}

	## change v	
	if ( (is.null(set_v))+(is.null(add_v))+(is.null(mult_v))+(is.null(rescale_v))+(is.null(fun_v)) != 5){
		vv=y[3, ]
		
		print_range=stats::quantile(vv, probs=c(0, 0.25, 0.5, 0.75, 1))
		cat("The original v are:\n")
		print(print_range)		
		
		if (!is.null(set_v)) vv=rep_len(set_v, nc)
		if (!is.null(add_v)) vv=vv+add_v
		if (!is.null(mult_v)) vv=vv*mult_v
		if (!is.null(rescale_v)) vv=RESCALE_FUN_VEC(vv, para=rescale_v)
		if (!is.null(fun_v)) vv=if (is.function(fun_v)) match.fun(fun_v)(vv) 
			else if (is.list(fun_v)) USE_INTERNAL_CURVE(vv, LIST=fun_v, cat_text=NULL)	
		
		if (checks==TRUE){
			which_v_big=which(vv>1)
			which_v_small=which(vv<0)
			if (length(which_v_big)>0){
				vv[which_v_big]=1
				if (warn==TRUE) warning("Some adjusted v values are larger than 1 and are set to 1.")
			}
			if (length(which_v_small)>0){
				vv[which_v_small]=0
				if (warn==TRUE) warning("Some adjusted v values are smaller than 0 and are set to 0.")
			}
		}
		
		y[3, ]=vv
		vv=NULL
	}
	
	if (alpha==FALSE){
		y=apply(y, 2, FUN=function(xx) grDevices::hsv(xx[1], xx[2], xx[3]))
	} else {
		y=rbind(y, alp_store)
		y=apply(y, 2, FUN=function(xx) grDevices::hsv(xx[1], xx[2], xx[3], alpha=xx[4]))
	}
	
	if ( (is.null(fun_h)) + (is.null(fun_s)) + (is.null(fun_v)) != 3) cat("Attention: when using internal curves, the function believes you have scaled h, s or v values into [0, 1] and does not check this.\n")
	
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

RESCALE_FUN_VEC=function(X, para) if ( ! is.function(para)) scales::rescale(X, to=para) else match.fun(para)(X)

USE_INTERNAL_CURVE=function(X, LIST, cat_text=NULL){
	if (! is.null(cat_text)) cat(cat_text)
	getfun=if (is.null(LIST[["FUN"]])) LIST[["fun"]] else LIST[["FUN"]]
	getfun=if (is.null(getfun)) LIST[[1]] else getfun

	stopifnot(!is.null(getfun) || is.character(getfun))
	if ( ! getfun %in% c("s", "c", "circle", "linear")) stop("Currently the internal curves only support functions designated by s, circle, c or linear.")
	
	# s curve
	if (getfun=="s"){
		Y=S_CURVE_TRANSFORM(
			X, 
			C1=if (is.null(LIST[["c1"]])) -2 else LIST[["c1"]], 
			C2=if (is.null(LIST[["c2"]])) 2 else LIST[["c2"]], 
			DOMAIN=LIST[["domain"]]
		)
	}
	
	# random point, though the name is linear
	if (getfun=="linear"){
		Y=RANDOM_POINT_TRANSFORM(
			X, 
			X0=if (is.null(LIST[["x0"]])) 0.4 else LIST[["x0"]], 
			Y0=if (is.null(LIST[["y0"]])) 0.6 else LIST[["y0"]]
		)
	}
	
	# circumcircle
	if (getfun %in% c("c", "circle")){
		Y=CIRCUMCIRCLE_TRANSFORM(
			X, 
			VALUE=if (is.null(LIST[["value"]])) 0.5 else LIST[["value"]]
		)
	}
	
	Y
}
	
S_CURVE_TRANSFORM=function(X, C1=-2, C2=2, DOMAIN=c(0, 1)){
	if (identical(DOMAIN, "range")) DOMAIN=range(X)
	C1=if (C1==0) 1 else if (C1<0) abs(C1)+1 else 1/(1+C1) 
	C2=if (C2==0) 1 else if (C2>0) 1+C2 else 1/(1+abs(C2))
	bigp=which(X>0.5)
	equalp=which(X==0.5)
	smallp=which(X<0.5)
	X[bigp]=(-((2-X[bigp]*2)^C2)/2)+1
	if (length(equalp)>0) X[equalp]=0.5
	X[smallp]=((X[smallp]*2)^C1)/2 
	if (is.null(DOMAIN)) DOMAIN=c(0, 1)
	if (! identical(DOMAIN, c(0, 1))) X=scales::rescale(c(0, 1, X), to=c(DOMAIN[1], DOMAIN[2]))[-c(1: 2)]
	X	
}

CIRCUMCIRCLE_TRANSFORM=function(X, VALUE){
	if (VALUE==0) stop ("VALUE should not be 0.")
	if (VALUE > 1 | VALUE <  (-1)) stop("VALUE must be between 1 and -1.")
	X0=0.5 - VALUE*0.19 # ensure X0 be not smaller than 0.31 and not bigger than 0.69
	Y0=1-X0 # currently only symetrical
	# below for R
	sidea=P1P2DIST(p1=c(1, 1), p2=c(X0, Y0))
	sideb=P1P2DIST(p1=c(0, 0), p2=c(X0, Y0)) # sidec=P1P2DIST(c(0, 0), c(1, 1))=1.414214
	pabc=(sidea+sideb+1.414214)/2
	R=(0.25*sidea*sideb*1.414214)/sqrt(pabc*(pabc-sidea)*(pabc-sideb)*(pabc-1.414214))
	# below for middle point of the circle
	midy=((((X0^2)+(Y0^2))/2)-X0)/(Y0-X0)
	midx=1-midy
	y=if (Y0>X0) sqrt((R^2)-((X-midx)^2))+midy else -sqrt((R^2)-((X-midx)^2))+midy
	round(y, 3) # further ensure not bigger than 1
}

P1P2DIST=function(p1, p2) sqrt(((p1[1]-p2[1])^2)+((p1[2]-p2[2])^2)) 

RANDOM_POINT_TRANSFORM=function(X, X0, Y0){
	tana1=Y0/X0
	smallp=which(X<X0)
	bigp=which(X>=X0)
	X[smallp]=X[smallp]*tana1
	tana2=(1-Y0)/(1-X0)
	X[bigp]=Y0+((X[bigp]-X0)*tana2)
	X
}
