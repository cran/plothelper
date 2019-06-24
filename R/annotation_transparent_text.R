#' Layer for Transparent Text
#' 
#' Suppose there is a colored rectangle
#' with some texts and 
#' you want the texts to be transparent so that 
#' the colors of the background can be seen. Now 
#' you can use this function. The function 
#' can be used as a ggplot layer or a generator 
#' of image. NOTE: when the function is 
#' used as a layer, it uses 
#' \code{ggplot2::annotation_raster} to 
#' do the drawing, so you must 
#' set limits for the x axis and the y axis. See examples.
#' 
#' @param label the text.
#' @param xmin the left side of the rectangle.
#' @param xmax the right side of the rectangle.
#' @param ymin the bottom side of the rectangle.
#' @param ymax the top side of the rectangle.
#' @param bg the colors of the rectangle. It can be 
#' a character vector of colors, a matrix of colors, 
#' an object of raster class or even a image 
#' read into R through \code{magick::image_read}.
#' Default is color black.
#' @param alpha it is only used 
#' when \code{bg} is a character 
#' vector. Default is 0.5.
#' @param interpolate when \code{bg} is 
#' a matrix, a image or 
#' a raster, this parameter is used and 
#' will be passed to \code{ggplot2::annotation_raster} 
#' to draw a colored rectangle. Default is TRUE.
#' @param result_interpolate whether to use interpolate 
#' in the final result. Default is TRUE.
#' @param expand sometimes 
#' it is needed to slightly expand the x position and 
#' y position to put the text so that they can be 
#' shown. It should be two values used by x and y
#' respectively. Default is 0.08 and 0.08.
#' @param family family of text. Default is SimHei 
#' which ensures that Chinese texts can be shown.
#' @param fontface fontface.
#' @param reflow whether to change lines 
#' automatically. It will be passed to 
#' \code{ggfittext::geom_fit_text}. Default is FALSE.
#' @param place position adjustment used by 
#' \code{ggfittext:;geom_fit_text}.
#' @param bg_trim whether to trim \code{bg}. Most 
#' of the time we do want to trim it. However, the 
#' \code{magick::image_trim} function sometimes 
#' trims wrongly. So you can turn it off. NOTE: the default 
#' value of \code{bg_trim} is NULL, which means 
#' DO NOT TRIM.
#' @param result when it is "layer", the function can be 
#' used as a ggplot layer. When it is "magick", the result 
#' is only an image which is created by the magick package.
#' @param width the width of 
#' the text rectange. It will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is 800.
#' @param height the height of the 
#' text rectange. It will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is NULL, which means 
#' it will be computed automatically.
#' @param res resolution in pixels which will be passed 
#' to \code{magick::image_graph}. Default is 72.
#'
#' @export
#' @import scales
#' @examples
#' \donttest{
#' # Example 1
#' m=matrix(rainbow(7), nrow=1)
#' ggplot()+coord_fixed()+
#' 	xlim(0, 7)+ylim(-2, 4)+theme_void()+
#' 	annotation_raster(
#' 		raster=m, 
#' 		xmin=0, ymin=-3, 
#' 		xmax=7, ymax=5, 
#' 		interpolate=TRUE
#' 	)+
#' 	annotation_transparent_text(
#' 		label="R\nDATA\nVISUALIZATION", 
#' 		xmin=0, xmax=7, 
#' 		ymin=-1, ymax=3, 
#' 		family="sans", fontface=2, alpha=0.8, 
#' 		place="left", expand=c(0.08, 0.02)
#' 	)
#' # 
#' # Example 2, this time the result is only an image.
#' tt=annotation_transparent_text(
#' 	label="abcdefg", 
#' 	xmin=1, xmax=8, 
#' 	ymin=1, ymax=4, 
#' 	alpha=0.6, 
#' 	result="magick"
#' )
#' #
#' # Example 3, the rectangle is a matrix.
#' m=colorRampPalette(c("yellow", "purple"))(10)
#' ggplot()+coord_fixed(expand=FALSE)+
#' 	theme(panel.background=element_rect(fill="red"))+
#' 	xlim(0, 9)+
#' 	ylim(0, 5)+
#' 	annotation_transparent_text(
#' 		label="hehehaha", 
#' 		xmin=1, xmax=8, 
#' 		ymin=1, ymax=4, 
#' 		bg=m, alpha=1
#' 	)
#' }
annotation_transparent_text=function(label, xmin, xmax, ymin, ymax, bg="black", alpha=0.5, interpolate=TRUE, result_interpolate=TRUE, expand=c(0.08, 0.08), family="SimHei", fontface=1, reflow=FALSE, place="center", bg_trim=NULL, result=c("layer", "magick"), width=800, height=NULL, res=72){
	
	result=result[1]
	stopifnot(result %in% c("layer", "magick"))
	bg_class=class(bg)[1]
	if (grepl("magick", bg_class)) bg_class="magick-image"
	if ( ! bg_class %in% c("character", "matrix", "raster", "magick-image", "gg")) stop("bg must be a character vector, a matrix, a  raster, a image or a gg object.")
	if (bg_class == "matrix"){
		if (! is.character(bg)) stop("When bg is a matrix, its elements must be names of colors. ")
	}
	if (bg_class == "character") bg=matrix(scales::alpha(bg, alpha), nrow=1)

	# expand
	if (length(expand) == 1) expand=rep(expand, 2)
	xexpand=(xmax-xmin)*expand[1]
	yexpand=(ymax-ymin)*expand[2]
	XMIN=xmin-xexpand
	XMAX=xmax+xexpand
	YMIN=ymin-yexpand
	YMAX=ymax+yexpand
	
	# text
	tegg=ggplot()+coord_fixed(xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX), expand=FALSE)+
		ggplot2::theme_void()+ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
		ggfittext::geom_fit_text(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, label=label), reflow=reflow, grow=TRUE, family=family, fontface=fontface, place=place)
	te_height=round(width*(YMAX-YMIN)/(XMAX-XMIN), 0)
	img_tegg=magick::image_graph(width=width, height=te_height, bg="transparent", res=res)
	print(tegg)
	grDevices::dev.off()

	# bg
	if (bg_class %in% c("character", "matrix", "raster")){
		bggg=ggplot2::ggplot()+ggplot2::coord_fixed(xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::annotation_raster(raster=bg, xmin=XMIN, ymin=YMIN, xmax=XMAX, ymax=YMAX, interpolate=interpolate)
		adj_height=if (is.null(height)) ADJUST_HEIGHT(W=width, XMIN=XMIN, XMAX=XMAX, YMIN=YMIN, YMAX=YMAX) else height
		img_bggg=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(bggg)
		grDevices::dev.off()	
		if ( ! is.null(bg_trim)) img_bggg=magick::image_trim(img_bggg, bg_trim)
		img_bggg=ReSiZe_tO_stAndArd(x=img_bggg, standard=img_tegg)
	} else if (bg_class == "magick-image"){
		if ( ! is.null(bg_trim)) bg=magick::image_trim(bg, bg_trim)
		img_bggg=ReSiZe_tO_stAndArd(x=bg, standard=img_tegg)
	} else if (bg_class == "gg"){
		if (is.null(height)){
			# raster here should have axis with expand=FALSE
			bg_gg_info=ggplot2::ggplot_build(bg)
			bg_gg_info=ggplot2::summarise_layout(bg_gg_info)
			bg_gg_info=as.numeric(bg_gg_info[1, c("xmin", "xmax", "ymin", "ymax")])
			adj_height=ADJUST_HEIGHT(W=width, XMIN=bg_gg_info[1], XMAX=bg_gg_info[2], YMIN=bg_gg_info[3], YMAX=bg_gg_info[4])
		} else {
			adj_height=height
		}
		img_bggg=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(bg)
		grDevices::dev.off()	
		if ( ! is.null(bg_trim)) img_bggg=magick::image_trim(img_bggg, bg_trim)
		img_bggg=ReSiZe_tO_stAndArd(x=img_bggg, standard=img_tegg)
	}	
	
	# composite
	# comp=magick::image_trim(image_composite(img_tegg, img_bggg, "out", "+0+0"))
	comp=magick::image_composite(img_tegg, img_bggg, "out", "+0+0") # DO NOT TRIM
	if (result == "magick"){
		comp
	} else {
		ggplot2::annotation_raster(comp, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, interpolate=result_interpolate)
	}
}
	
ReSiZe_tO_stAndArd=function(x, standard){
	sinfo=as.numeric(magick::image_info(standard)[1, 2: 3])
	ssize=paste(sinfo[1], "x", sinfo[2], "!", sep="")
	magick::image_resize(x, ssize)
}

ADJUST_HEIGHT=function(W, XMIN, XMAX, YMIN, YMAX) floor(W*((YMAX-YMIN)/(XMAX-XMIN)))
