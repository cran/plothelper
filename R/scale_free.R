#' Scale values into a Certain Location
#'
#' A simple function to put numeric values into 
#' a certain interval. Suppose you have 
#' 20, 60, 80, 100, and you want them to be in the 
#' interval of [0, 1], so you can get 0, 0.5, 0.75, 1.
#'
#' @param x a numeric vector or a numeric matrix, 
#' data frame, tibble object.
#' @param left the smallest value of the the interval. 
#' If \code{x} has n columns, then \code{left}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#' @param right the largest value of the the interval. 
#' If \code{x} has n columns, then \code{right}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#' @param reverse whether to assign values in a 
#' reverse way. Default is FALSE. 
#' If \code{x} has n columns, then \code{reverse}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#'
#' @export
#' @examples
#' y=scale_free(c(-1, 0, 2))
#' y=scale_free(c(-1, 0, 2), rev=TRUE)
#' #
#' # x is a data frame.
#' x=data.frame(
#' 	c(-1, 0, 0, 0, 2), c(-1, 0, 0, 0, 2), 
#' 	c(-2, 0, 2, 4, 6), c(-2, 0, 2, 4, 6)
#' )
#' y=scale_free(x, 
#' 	left=0, right=10, 
#' 	reverse=c(FALSE, TRUE, FALSE, TRUE)
#' )
#' y=scale_free(x, 
#' 	left=c(0, 0, 100, 100), right=c(10, 100, 200, 200), 
#' 	reverse=c(FALSE, TRUE, FALSE, TRUE)
#' )
scale_free=function(x, left=0, right=1, reverse=FALSE){
	cla=class(x)[1]
	if ( ! cla %in% c("matrix", "data.frame", "tbl_df") & is.numeric(x) == FALSE) stop("x must be a numeric vector or a matrix, data frame, tibble object with numeric values.")
	if (! is.numeric(left)||! is.numeric(right)) stop("x, left, right must be numeric!")	
	if (! cla %in% c("matrix", "data.frame", "tbl_df")){
		if(left[1] >= right[1]) stop("left must be smaller than right!")
		res=scAlE_frEE(X=x, LEFT=left[1], RIGHT=right[1], REVERSE=reverse[1])
	} else {
		nc=ncol(x)
		left=rep_len(left, length.out=nc)
		right=rep_len(right, length.out=nc)
		reverse=rep_len(reverse, length.out=nc)
		if (length(which(left >= right)) > 0) stop("left must be smaller than right!")	
		res=x
		for (i in 1: nc) res[i]=scAlE_frEE(X=res[i], LEFT=left[i], RIGHT=right[i], REVERSE=reverse[i])
	}
	res
}	
		
scAlE_frEE=function(X, LEFT=0, RIGHT=1, REVERSE=FALSE){
	if (REVERSE==TRUE) X=X*(-1)
	maxn=max(X)
	minn=min(X)
	pos=(X-minn)/(maxn-minn)
	pos=(RIGHT-LEFT)*pos
	pos=pos+LEFT
	pos
}
