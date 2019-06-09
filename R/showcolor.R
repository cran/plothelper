#' Show a Color Palette
#' 
#' Simple function to show colors. NOTE: 
#' do not add \code{coord_flip()}.
#'
#' @param x a chacracter vector of colors.
#' @param label_size size of text on 
#' x-axis to show color names.
#' @param ... other arguments passed to 
#' \code{geom_bar}.
#'
#' @export
#' @examples
#' # A palette used by David Hockney
#' co=c("#833822", "#C03800", "#D3454C", 
#' 	"#DC6A30", "#F29856", "#FEEF70", 
#' 	"#A5D56D", "#16D670", "#00932F", 
#' 	"#03592E", "#04B7B0", "#007BA9", 
#' 	"#EC46BF", "#6A2C8F"
#' )
#' showcolor(co, label_size=10)
showcolor=function(x, label_size=15, ...){
	lenx=length(x)
	withname_slash_n=add_slash_n(x, delete_space=FALSE, vertical_line=FALSE)
	withname_slash_n=paste(1: lenx, withname_slash_n, sep="\n \n")
	raw_name=paste(1: lenx, x)
	cat("The colors are: \n")
	for (i in raw_name) cat(i, "\n")
	ggplot()+
		geom_bar(show.legend=FALSE, stat="identity", aes(x=1: lenx, y=rep(1, lenx)), fill=x, ...)+
		scale_x_continuous(labels=withname_slash_n, breaks=1: lenx, expand=expand_scale(c(0, 0)))+
		scale_y_continuous(limits=c(0, 1), expand=expand_scale(c(0, 0)))+
		theme(
			panel.background=element_blank(), 
			panel.grid=element_blank(),
			axis.ticks=element_blank(),
			axis.title=element_blank(), 
			axis.text.y=element_blank(), 
			axis.text.x=element_text(size=label_size)
		)
}
