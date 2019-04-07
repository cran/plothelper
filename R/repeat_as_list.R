#' Replicating Any Object to Form a List
#'
#' This is a convenient function to replicate an 
#' object to form a list (and to give names to 
#' its elements).
#'
#' @param x any object to be replicate. It can be 
#' a vector, a data frame, NA, NULL, etc. 
#' @param times a single integer 
#' giving the number of times to repeat x.
#' @param labels the names for the elements of 
#' the output list. Its lengths should be equal to 
#' argument \code{times}. Default is NULL which 
#' means no name is added.
#'
#' @export
#' @examples
#' a=data.frame(x=1: 3, y=4: 6)
#' aa=repeat_as_list(a, 2, labels=c("d1", "d2"))
repeat_as_list=function(x, times=1, labels=NULL){
	y=rep_len(list(x), times)
	if (! is.null(labels)){
		stopifnot(length(labels)==times)
		names(y)=labels
	}
	y
}
