#' format the theme of plot
#'
#' Format the theme of plot.
#'
#' @param p a ggplot-format plot.
#' @param fsz font size in plot.
#' @param ff font family in plot.
#' @param lsz line size of panel border and axis in plot.
#' @param tkl tick length in plot.
#' @return a plot with a new theme. 

#' @export
#' @import ggplot2 

fm=function(p, fsz=13, ff="TT Arial", lsz=0.5, tkl=0.2){
	fm1= theme_bw()
	fm2= theme(axis.text = element_text(size=fsz,family=ff), text = element_text(size=fsz,family=ff), legend.text = element_text(size=fsz,family=ff))
	fm3=theme(panel.border = element_rect(fill=NA,color="black", size=unit(lsz,"cm"), linetype="solid"), axis.line = element_line(colour = "black", size = unit(lsz,"cm")),axis.ticks = element_line(colour = "black", size = unit(lsz,"cm")),
	axis.ticks.length=unit(tkl,"cm"),legend.box.margin=margin(-10,-10,-10,-10))
	p=p+fm1+fm2+fm3
	return(p)
}