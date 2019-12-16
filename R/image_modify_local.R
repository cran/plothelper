#' Modify Only a Subregion of an Image
#' 
#' The function allows you to modify a subregion  
#' of your image (or, the opposite, keep the 
#' subregion unchanged while modifying other parts).
#' You can set the four sides of the subregion by 
#' mouse click.
#' 
#' @param x an image read into R by 
#' \code{magick::image_read} or an image  
#' modified by functions in the magick 
#' package.
#' @param FUN the function used to 
#' modify \code{x}. NOTE: the result 
#' of \code{FUN} must be of the same 
#' class as \code{x} and its width and 
#' height must not be changed during 
#' modification.
#' @param geometry this parameter is 
#' different from the one used in package 
#' magick. Here, in this function, you can 
#' set \code{geometry = "click"} if you want 
#' to show which part you want to modify 
#' by mouse click (see 
#' function \code{image_crop_click} for how to 
#' use mouse click). Otherwise, you can use 
#' a length 4 vector with the exact order: left, 
#' right, top, bottom. 
#' @param local if it is 1 or "local", only a subregion 
#' of your image will be modified. If it is 2
#' or "other", keep the subregion unchanged while 
#' modifying other parts. If it is 3 or "subregion", 
#' the result is only the modified subregion, not the 
#' whole image.
#' @param ..., extra parameters used by 
#' \code{FUN}.
#' 
#' @export
image_modify_local=function(x, FUN, geometry="click", local="local", ...){
	stopifnot(is.function(FUN))
	myfun=match.fun(FUN)
	if (identical(geometry, "click")){
		POS=image_crop_click(x, only_value=TRUE)
	} else {
		if (length(geometry) != 4 || geometry[2]-geometry[1] <= 0 || geometry[4]-geometry[3] <= 0) stop("geometry must be either click or a integer vector of length 4 with the excact order of left, right, top, bottom.")
		POS=geometry
	}
	cha=paste(POS[2]-POS[1]+1, "x", POS[4]-POS[3]+1, "+", POS[1]-1, "+", POS[3]-1, sep="")
	small=magick::image_crop(x, geometry=cha)

	if (identical(local, 1) | identical(local, "local")){
		small=myfun(small, ...)
		res=magick::image_composite(x, small, operator="atop", offset=paste("+", POS[1]-1, "+", POS[3]-1, sep=""))
	} else if (identical(local, 2) | identical(local, "other")){
		x=myfun(x, ...)
		res=magick::image_composite(x, small, operator="atop", offset=paste("+", POS[1]-1, "+", POS[3]-1, sep=""))
	} else if (identical(local, 3) | identical(local, "subregion")){
		res=myfun(small, ...)
	} else {
		stop("Argument local must be 1, 2 or 3.")
	}
	res
}
