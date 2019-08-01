#' Checking Min, Max, Labels and Label Positions
#'
#' Given a numeric vector or a ggplot object, the function
#' will check the range, labels and label 
#' positions (the same as major grid lines) that 
#' will used on the axis. The result is a length 5 list for 
#' min limit, max limit, labels, major grid line positions, 
#' all (major and minor) grid line positions.
#' 
#' @param a extreme values of a numeric vector. Note: only 
#' one of \code{a}, \code{v}, \code{gg} can be non-NULL.
#' @param b another extreme value if \code{a} is not NULL.
#' @param v a numeric vector.
#' @param gg a gg object created by ggplot function.
#' Which value will be checked depends on \code{axis}.
#' @param mult default is 0.05 and should be of length 
#' 1 or 2. It mimics the \code{mult} argument of 
#' \code{ggplot2::expand_scale}. It is only used when 
#' a is numeric or v is non-NULL.
#' @param add default is 0. It mimics the \code{add} 
#' argument of \code{ggplot2::expand_scale}.
#' @param axis if \code{gg} is used or \code{a} is a 
#' ggplot object, 
#' which axis will 
#' be checked? It can be "x" or "y" (default).
#'
#' @export
#' @examples
#' # The following three have the same results.
#' get_gg_label(a=1, b=1000, mult=0)
#' get_gg_label(v=c(1, 500, 1000), mult=0)
#' p=ggplot()+geom_point(aes(1: 3, c(1, 500, 1000)))+
#'   scale_y_continuous(expand=expand_scale(mult=0))
#' get_gg_label(gg=p)
get_gg_label=function(a=NULL, b=NULL, v=NULL, gg=NULL, mult=0.05, add=0, axis="y"){
	## expand_scale use the folloing method for numeric:
	## Suppose min=3, max=10, mult=c(1.5, 1.8), add=c(5, 6), then
	## the dif=10-3=7
	## x left range=3-7*1.5-5
	## x right range=10+7*1.8+6
	
	stopifnot(axis %in% c("x", "y"))	
	
	a_is_gg=if (class(a)[1] == "gg") TRUE else FALSE

	if (is.null(a) + is.null(v) + is.null(gg) != 2) stop("One and only one of a, v, gg must not be NULL.")
	
	if (is.null(gg) & a_is_gg == FALSE){
		if (is.null(a)){
			a=min(v, na.rm=TRUE)
			b=max(v, na.rm=TRUE)
		}
		gg=ggplot2::ggplot()+ggplot2::geom_blank(aes(x=c(1, 2), y=c(a, b)))+ggplot2::scale_y_continuous(expand=ggplot2::expand_scale(mult=mult, add=add))
	}
	panel_info=ggplot2::ggplot_build(plot=if (a_is_gg == TRUE) a else gg)$layout$panel_params[[1]]
	
	if (is.null(gg) & a_is_gg == FALSE){
		list(min=panel_info$y.range[1], 
			max=panel_info$y.range[2], 
			label=as.character(panel_info$y.labels), 
			position=panel_info$y.major_source, 
			all=panel_info$y.minor_source
		)
	} else {
		if (axis == "y"){
			list(min=panel_info$y.range[1], 
				max=panel_info$y.range[2], 
				label=as.character(panel_info$y.labels), 
				position=panel_info$y.major_source, 
				all=panel_info$y.minor_source
			)
		} else {
			list(min=panel_info$x.range[1], 
				max=panel_info$x.range[2], 
				label=as.character(panel_info$x.labels), 
				position=panel_info$x.major_source, 
				all=panel_info$x.minor_source
			)			
		}
	}
}
	