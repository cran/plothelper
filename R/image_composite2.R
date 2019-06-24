#' Centering Two Images and Creating a Composite Image
#' 
#' The function is a convenient wrapper of 
#' \code{magick::image_composite}. However, 
#' the \code{offset} parameter of the latter is 
#' difficult to handle. The function centers a 
#' image and b image in different ways and then 
#' combines them. See the \code{offset} parameter
#' off this function.
#'
#' @param a image.
#' @param b another image.
#' @param operator this character will be passed to 
#' \code{magick::image_composite}. It can be "in", "out", 
#' "atop", "blend", etc.Default is "in". For instance, if it 
#' is "in", then a part of image b is kept and the shape of 
#' this part is the same as the shape of image a. If it is 
#' "out", a hole with the shape of a will be left.
#' @param offset how to put a and b together in order to 
#' combine them. If it is written in the format like "+0+0", 
#' then it will be passed to \code{magick::image_composite}
#' directly. However, it also can be the following values: 
#' \itemize{
#'   \item (1) middle or center: which is the default. 
#' Now a is put at the center of b.
#'   \item (2) samea: adjust the size of b according to 
#' a and then combine them.
#'   \item (3) sameb: adjust the size of a according to 
#' b and then combine them.
#'   \item (4) height: adjust the height of a according to
#' the height of b.
#'   \item (5) width: adjust the width of a according to 
#' the width of b.
#'   \item (6) a>b: this is used only when a is bigger than 
#' b and you want to blend a with b.
#' }
#' @param compose_args this will be passed 
#' to \code{magick::image_composite}.
#' @param scale whether to keep ratio in resizing a 
#' when \code{offset} is "width" or "height". 
#' Default is TRUE.
#'
#' @export
image_composite2=function(a, b, operator="in", offset="middle", compose_args="", scale=TRUE){
	if (scale == TRUE){
		kept=""
	} else if (scale == FALSE){
		kept="!"
	} else {
		stop("scale must be either TRUE or FALSE.")
	}
	a_info=as.numeric(magick::image_info(a)[1, 2: 3])
	b_info=as.numeric(magick::image_info(b)[1, 2: 3])
	a_width=a_info[1]
	a_height=a_info[2]
	b_width=b_info[1]
	b_height=b_info[2]
	
	if (offset %in% c("middle", "center")){
		if (a_width < b_width & a_height < b_height){ 
			xpos=ODD_ADJUST(a_width, b_width)
			ypos=ODD_ADJUST(a_height, b_height)
			pos=paste("-", xpos, "-", ypos, sep="")
			final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)
		} else {
			if (a_width == b_width & a_height == b_height){
				final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)
				message("Do not need resizing.")
			}
			if (a_width > b_width & a_height > b_height){
				b=ReSiZe_tO_stAndArd(b, standard=a)
				final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)
				message("b is neither wider nor higher than a, so resize b according to a and then combining them.")
			} else if (a_width < b_width & a_height >= b_height){
				resizing=paste(b_width, "x", a_height, "!", sep="")
				b=magick::image_resize(b, resizing)
				xpos=ODD_ADJUST(a_width, b_width)
				pos=paste("-", xpos, "+0", sep="")
				final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)
				message("b is not higher than a, so resize b according to a' height and then combining them.")
			} else if (a_width >= b_width & a_height < b_height){
				resizing=paste(a_width, "x", b_height, "!", sep="")
				b=magick::image_resize(b, resizing)
				ypos=ODD_ADJUST(a_height, b_height)
				pos=paste("+0", "-", ypos, sep="")
				final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)
				message("b is not wider than a, so resize b according to a' width and then combining them.")
			}
		}
	} else if (offset == "samea"){
		b=ReSiZe_tO_stAndArd(x=b, standard=a)
		final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)
	} else if (offset == "sameb"){
		a=ReSiZe_tO_stAndArd(x=a, standard=b)
		final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)		
	} else if (offset == "height"){
		a=magick::image_resize(a, paste("x", b_height, kept, sep=""))
		a_width=as.numeric(magick::image_info(a)[1, 2])
		if (a_width < b_width){
			xpos=ODD_ADJUST(a_width, b_width)
			pos=paste("-", xpos, "+0", sep="")
			final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)
		} else {
			a=ReSiZe_tO_stAndArd(x=a, standard=b)
			final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)
			message("b is not wider than a, so resize a according to b and then combining them.")
		}
	} else if (offset == "width"){
		a=magick::image_resize(a, paste(b_width, "x", kept, sep=""))
		a_height=as.numeric(magick::image_info(a)[1, 3])
		if (a_height < b_height){
			ypos=ODD_ADJUST(a_height, b_height)
			pos=paste("+0", "-", ypos, sep="")
			final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)		
		} else {
			a=ReSiZe_tO_stAndArd(x=a, standard=b)
			final=magick::image_composite(a, b, operator=operator, offset="+0+0", compose_args=compose_args)
			message("b is not higher than a, so resize a according to b and then combining them.")
		}
	} else if (offset == "a>b"){
		if (a_width < b_width | a_height < b_height) stop("When offset is a>b, a must both wider and higher than b.")
		xpos=abs(ODD_ADJUST(a_width, b_width))
		ypos=abs(ODD_ADJUST(a_height, b_height))
		pos=paste("+", xpos, "+", ypos, sep="")
		final=magick::image_composite(a, b, operator=operator, offset=pos, compose_args=compose_args)
	} else {
		final=magick::image_composite(a, b, operator=operator, offset=offset, compose_args=compose_args)
	}
	
	final
}

ODD_ADJUST=function(small, big){
	ss=small %% 2
	bb=big %% 2
	if (ss == 0 & bb == 0){
		(big/2)-(small/2)
	} else if (ss == 1 & bb == 0){
		(big/2)-ceiling(small/2)
	} else if (ss == 0 & bb == 1){
		floor((big/2)-(small/2))
	} else if (ss == 1 & bb == 1){
		(big/2)-(small/2)
	}
}
