#' Layer for Drawing a Single 
#' Irregular Polygon 
#' with Shading Colors
#' 
#' \code{ggplot2::annotation_raster} can only 
#' draw shading rectangles. However, this 
#' function can draw polygons of any shape 
#' with shading colors. See the \code{shape} 
#' argument and the \code{raster} argument.
#'
#' @param shape the polygon can be 
#' a data frame (or matrix object, or tbl_df object) 
#' with x and y coordinates, 
#' a plot created by ggplot or a image 
#' read into R by \code{magick::image_read}. 
#' @param xmin the left side of the position to 
#' put the polygon. When 
#' \code{shape} is something like a data frame, 
#' you do not need to set xmin, xmax, ymin and ymax, 
#' for the function will generate these values according 
#' to the coordinates in the polygon.
#' @param xmax the right side. 
#' @param ymin the bottom side.
#' @param ymax the top side.
#' @param raster the shading colors. 
#' It can be a raster object, 
#' a matrix of colors, a ggplot plot or an 
#' image read into R by 
#' \code{magick::image_read}.
#' @param interpolate the \code{interpolate}
#' argument used by \code{ggplot2::annotation_raster}
#' when the \code{raster} argument is a matrix or 
#' raster.
#' @param result_interpolate whether to interpolate in the 
#' final result which is essentially an output of 
#' \code{ggplot2::annotation_raster}. Default is TRUE.
#' @param shape_trim this argument 
#' decides whether to trim edges 
#' of \code{shape}.
#' It should be a number 
#' between 0 and 100. Default is NULL. If it is NULL, 
#' no trimming will be done. 
#' @param raster_trim whether to trim raster. 
#' Most of the time we do want to trim the raster.
#' However, the \code{magick::image_trim} function 
#' sometimes trims wrongly. So you may want to turn 
#' it off. Default is NULL.
#' @param result_trim how to trim the 
#' final result. If you find your 
#' figure loses some parts, you can try to turn this off. 
#' Default is NULL.
#' @param result when it is "layer", the function is a  
#' ggplot layer. When it is "magick", the function only 
#' create an image.
#' @param width the width which will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is 800. HOWEVER, if the 
#' final polygon has fuzzy edges, try to enlarge \code{width} 
#' to make them look better.
#' @param height the height which will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is NULL, which means 
#' it will be computed automatically.
#' @param res resolution in pixels which will be passed to 
#' \code{magick::image_graph}. Default is 72.
#'
#' @export
#' @examples
#' \donttest{
#' # Example 1
#' poly=ellipsexy(-1, 0, a=1, b=1)
#' m=matrix(rainbow(7))
#' ggplot()+
#' 	coord_fixed(expand=FALSE)+
#' 	xlim(-3, 6)+ylim(-1.5, 1.5)+
#' 	annotation_shading_polygon(
#' 		poly, 
#' 		raster=m
#' 	)+
#' 	annotation_shading_polygon(
#' 		poly, 
#' 		xmin=1, xmax=5, 
#' 		ymin=-1, ymax=1, 
#' 		raster=m
#' 	)
#' #
#' # Example 2, only an image
#' tt=annotation_shading_polygon(
#' 	poly, 
#' 	result="magick", 
#' 	width=280, height=280
#' )
#' #
#' # Example 3, both shape and raster are 
#' # ggplot plots.
#' p1=ggplot()+coord_fixed()+
#' 	geom_tile(aes(x=1: 5, y=1: 5))
#' p2=ggplot()+theme_void()+
#' 	coord_cartesian(expand=FALSE)+
#' 	geom_polygon(aes(x=c(0, 1, 1, 0), 
#' 		y=c(0, 0, 1, 1)), fill="red")
#' ggplot()+xlim(0, 11)+ylim(0, 6)+coord_fixed()+
#' 	annotation_shading_polygon(
#' 		shape=p1, 
#' 		xmin=1, xmax=10, 
#' 		ymin=1, ymax=5, 
#' 		raster=p2
#' 	)
#' }
annotation_shading_polygon=function(shape=data.frame(c(-1, 1, 0), c(0, 0, 1.732)), xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, raster=NULL, interpolate=TRUE, result_interpolate=TRUE, shape_trim=NULL, raster_trim=NULL, result_trim=NULL, result=c("layer", "magick"), width=800, height=NULL, res=72){

	result=result[1]
	stopifnot(result %in% c("layer", "magick"))
	if (is.null(raster)) raster=matrix(grDevices::rainbow(7))
	
	# check class of shape
	cla_shape=class(shape)[1]
	if (cla_shape %in% c("matrix", "data.frame", "tbl_df")){
		if (is.matrix(shape)) shape=data.frame(shape)
		if (ncol(shape) < 2) stop("shape must have at least 2 columns and only the first two columns will be used.")
		if (ncol(shape) > 2) shape=shape[, 1: 2]
		for (i in 1: 2){
			if (any(is.na(shape[, i]))) stop("shape must not have NAs.")
		}
		if (nrow(shape) < 3) stop("shape must have at least 3 rows after deleting NAs.")
	}
	if(grepl("magick", cla_shape)) cla_shape="magick-image"
	if (! cla_shape %in% c("data.frame", "tbl_df", "magick-image", "gg")) stop("shape must be of class matrix, data.frame, tbl_df, magick-image or gg.")	
	
	# check class of raster
	cla_raster=class(raster)[1]
	if(grepl("magick", cla_raster)) cla_raster="magick-image"
	if (! cla_raster %in% c("matrix", "raster", "magick-image", "gg")) stop("raster must be of class matrix, raster, magick-image or gg.")
	
	# shape
	if (cla_shape %in% c("matrix", "data.frame", "tbl_df")){
		poly_x_min=min(shape[, 1])
		poly_x_max=max(shape[, 1])
		poly_y_min=min(shape[, 2])
		poly_y_max=max(shape[, 2])
		if (is.null(xmin)){
			xmin=poly_x_min
			xmax=poly_x_max
			ymin=poly_y_min
			ymax=poly_y_max
		}
		myshape=ggplot2::ggplot()+
			ggplot2::coord_cartesian(xlim=c(poly_x_min, poly_x_max), ylim=c(poly_y_min, poly_y_max), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::geom_polygon(aes(x=shape[, 1], y=shape[, 2]), color="black", fill="black")
		adj_height=if (is.null(height)) ADJUST_HEIGHT(W=width, XMIN=poly_x_min, XMAX=poly_x_max, YMIN=poly_y_min, YMAX=poly_y_max) else height
		img_shape=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(myshape)
		grDevices::dev.off()
		if (! is.null(shape_trim)) img_shape=magick::image_trim(img_shape, fuzz=shape_trim)	
		# img_shape=magick::image_trim(img_shape, fuzz=0)	
	}
	if (cla_shape == "gg"){
		# raster provided by users here should have axis with expand=FALSE
		if (result == "layer"){
			if (is.null(xmin)) stop("When shape is of class gg, xmin, xmax, ymin, ymax must not be NULL.")
		}
		shape=shape+ggplot2::theme_void()
		if (is.null(height)){
			shape_gg_info=ggplot2::summarise_layout(ggplot2::ggplot_build(shape))
			shape_gg_info=as.numeric(shape_gg_info[1, c("xmin", "xmax", "ymin", "ymax")])
			adj_height=ADJUST_HEIGHT(W=width, XMIN=shape_gg_info[1], XMAX=shape_gg_info[2], YMIN=shape_gg_info[3], YMAX=shape_gg_info[4])
		} else {
			adj_height=height
		}		
		img_shape=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(shape)
		grDevices::dev.off()
		if (! is.null(shape_trim)) img_shape=magick::image_trim(img_shape, fuzz=shape_trim)	
	}
	if (cla_shape == "magick-image"){
		if (result == "layer"){
			if (is.null(xmin)) stop("When shape is of class magick-image, xmin, xmax, ymin, ymax must not be NULL.")
		}
		img_shape=if (! is.null(shape_trim)) magick::image_trim(shape, fuzz=shape_trim) else shape
	}
	
	shape_info=as.numeric(magick::image_info(img_shape)[1, 2: 3])
	# recalculate width
	width=shape_info[1]
	
	# coord for raster when shape is not df
	if (cla_shape %in% c("magick-image", "gg")){
		poly_x_min=0
		poly_x_max=shape_info[1]
		poly_y_min=0
		poly_y_max=shape_info[2]
	}
	
	# raster
	if (cla_raster  %in% c("raster", "matrix")){
		bggg=ggplot2::ggplot()+
			ggplot2::coord_cartesian(xlim=c(poly_x_min, poly_x_max), ylim=c(poly_y_min, poly_y_max), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::annotation_raster(raster=raster, xmin=poly_x_min, xmax=poly_x_max, ymin=poly_y_min, ymax=poly_y_max, interpolate=interpolate)
		adj_height=if (is.null(height)) ADJUST_HEIGHT(W=width, XMIN=poly_x_min, XMAX=poly_x_max, YMIN=poly_y_min, YMAX=poly_y_max) else height 
		img_raster=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(bggg)
		grDevices::dev.off()
		if (! is.null(raster_trim)) img_raster=magick::image_trim(img_raster, fuzz=raster_trim)
		img_raster=ReSiZe_tO_stAndArd(x=img_raster, standard=img_shape)
	} 
	if (cla_raster == "gg"){
		# raster here should have axis with expand=FALSE
		if (is.null(height)){
			raster_gg_info=ggplot2::summarise_layout(ggplot2::ggplot_build(raster))
			raster_gg_info=as.numeric(raster_gg_info[1, c("xmin", "xmax", "ymin", "ymax")])
			adj_height=ADJUST_HEIGHT(W=width, XMIN=raster_gg_info[1], XMAX=raster_gg_info[2], YMIN=raster_gg_info[3], YMAX=raster_gg_info[4])
		} else {
			adj_height=height
		}
		img_raster=magick::image_graph(width=width, height=adj_height, bg="transparent", res=res)
		print(raster)
		grDevices::dev.off()
		if (! is.null(raster_trim)) img_raster=magick::image_trim(img_raster, fuzz=raster_trim)
		img_raster=ReSiZe_tO_stAndArd(x=img_raster, standard=img_shape)		
	}
	if (cla_raster == "magick-image"){
		if (! is.null(raster_trim)) raster=magick::image_trim(raster, fuzz=raster_trim)
		img_raster=ReSiZe_tO_stAndArd(x=raster, standard=img_shape)		
	}		

	# composite
	if (! is.null(result_trim)){
		comp=magick::image_trim(magick::image_composite(img_shape, img_raster, "in", "+0+0"), fuzz=result_trim)
	} else {
		comp=magick::image_composite(img_shape, img_raster, "in", "+0+0")
	}
	if (result == "magick"){
		comp
	} else {
		ggplot2::annotation_raster(raster=comp, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, interpolate=result_interpolate)
	}
}
