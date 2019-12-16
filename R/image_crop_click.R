#' Cuts out a Subregion of an Image by Mouse Click
#' 
#' This function is a wrapper of 
#' \code{magick::image_crop}. While the 
#' latter asks you to set a \code{geometry} 
#' parameter, this function enables you 
#' to set the four sides of a subregion only 
#' by click the mouse. You must click at 
#' least 2 times (that is, click on 2 different
#' points to define a rectangle). After clicking, 
#' please press Esc on your keyboard.
#' 
#' @param x an image read into R by 
#' \code{magick::image_read} or an image  
#' modified by functions in the magick 
#' package.
#' @param only_value the default is FALSE, 
#' which will return the subregion. If you set it to 
#' TRUE, the result is only four values with 
#' the order: left, right, top, bottom.
#' 
#' @export
image_crop_click=function(x, only_value=FALSE){
	if (grDevices::dev.capabilities()$locator == FALSE) stop("Your device does not support mouse locator.")
	y=grDevices::as.raster(x)
	# omar=graphics::par()$mar
	# graphics::par(mar=rep(0.5, 4))
	# on.exit(graphics::par(mar=omar))
	width=dim(y); height=width[1]; width=width[2]
	graphics::plot(y)
	clicking=graphics::locator()
	if (length(clicking$x) < 2) stop("You must click at least 2 times.")
	xrng=ceiling(range(clicking$x))
	yrng=ceiling(range(clicking$y))	
	if (xrng[1]<1) xrng[1]=1
	if (xrng[2]>width) xrng[2]=width
	if (yrng[1]<1) yrng[1]=1
	if (yrng[2]>height) yrng[2]=height
	top=height+1-yrng[2]
	bottom=height+1-yrng[1]
	res=c(left=xrng[1], right=xrng[2], top=top, bottom=bottom)
	cat(paste("The area is: \n", "left: ", res[1], "\n", "right: ", res[2], "\n", "top: ", res[3], "\n", "bottom: ", res[4], "\n", sep=""))
	if (only_value==TRUE){
		return(res)
	} else {
		cha=paste(res[2]-res[1]+1, "x", res[4]-res[3]+1, "+", res[1]-1, "+", res[3]-1, sep="")
		res=magick::image_crop(x, geometry=cha)
		return(res)
	}
}
