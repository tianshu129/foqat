#' Plot time series in batch
#'
#' Easy way to plot time series in batch.
#'
#' @param df dataframe of time series.
#' @param xlab text expression of x axis label. The default vaule is NULL. 
#' @param ylab text expression of y axis label. The default vaule is NULL.
#' @param cclist vector, colors of lines. The default vaule is NULL. 
#' @param bquote logical value. Set to TRUE if you want to use bquote in labs (xlab and y lab). The default vaule is FALSE.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the breaks specified by `date_breaks`
#'   - A `Date`/`POSIXct` vector giving positions of breaks
#'   - A function that takes the limits as input and returns breaks as output
#' @param date_breaks A string giving the distance between breaks like "2
#'   weeks", or "10 years". If both `breaks` and `date_breaks` are
#'   specified, `date_breaks` wins.
#' @param date_minor_breaks A string giving the distance between minor breaks
#'   like "2 weeks", or "10 years". If both `minor_breaks` and
#'   `date_minor_breaks` are specified, `date_minor_breaks` wins.
#' @param minor_breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the breaks specified by `date_minor_breaks`
#'   - A `Date`/`POSIXct` vector giving positions of minor breaks
#'   - A function that takes the limits as input and returns minor breaks as
#'     output
#' @param date_labels A string giving the formatting specification for the
#'   labels. Codes are defined in [strftime()]. If both `labels`
#'   and `date_labels` are specified, `date_labels` wins.
#' @param labels One of:
#'   - `NULL` no labels
#'   - `waiver()` for the default labels computed by the transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels as output. 
#'     Also accepts rlang lambda function notation.
#' @param expand For position scales, a vector of range expansion constants used
#'  to add some padding around the data to ensure that they are placed some 
#' distance away from the axes. Use the convenience function expansion() to 
#' generate the values for the expand argument. The defaults are to expand the 
#' scale by 5% on each side for continuous variables, and by 0.6 units on each 
#' side for discrete variables.
#' @param panelgap gap of panels. The default vaule is 0.5.
#'
#' @export
#' @examples
#' \dontrun{ 
#' #example 1
#' geom_ts_batch(aqi)
#' #example 2
#' xlab1="Time"
#' ylab1=c("NO","NO2","CO","SO2","O3")
#' geom_ts_batch(aqi, xlab=xlab1, ylab=ylab1)
#' #example 3
#' xlab2=bquote(Time~"")
#' ylab2=list(bquote(NO~" "~(ppbv)), bquote(NO[2]~"
#'  "~(ppbv)), bquote(CO~" "~(ppmv)), bquote(SO[2]~"
#'  "~(ppbv)), bquote(O[3]~" "~(ppbv)))
#' cclist=c("#eb2f96", "#1890ff", "#52c41a", "#faad14", "#f5222d")
#' geom_ts_batch(aqi, xlab=xlab2, ylab=ylab2, cclist=cclist, bquote=TRUE)
#' }
#' @importFrom scales hue_pal
#' @import patchwork 
#' @importFrom ggplot2 theme

geom_ts_batch<-function(
df,
xlab=NULL, 
ylab=NULL, 
cclist=NULL,
bquote=FALSE, 
breaks = waiver(),
date_breaks = waiver(),
labels = waiver(),
date_labels = waiver(),
minor_breaks = waiver(),
date_minor_breaks = waiver(),
expand = c(0,0),
panelgap=1
){
	if(length(cclist)==0){cclist=hue_pal()(ncol(df)-1)}
	for(i in 2:ncol(df)){
		plotdf=df[c(1,i)]
		if(length(xlab)==0){xlab=names(df)[1]}
		if(length(ylab)==0){ylab=names(df)[-1]}
		if(bquote==FALSE){
			px=geom_ts(df=plotdf, yl=2, yr=NULL, 
			yllab=ylab[i-1], yrlab=NULL, xlab=xlab,
			llist=2, plist=2, 
			llab=NULL, plab=NULL, alab=NULL, blab=NULL,
			ltype=NULL, pshape=NULL, 
			lsize=1, psize=1,
			lcc=c(cclist[i-1]), pcc=c(cclist[i-1]))+
			scale_x_datetime(
			  breaks = breaks,
			  date_breaks = date_breaks,
			  labels = labels,
			  date_labels = date_labels,
			  minor_breaks = minor_breaks,
			  date_minor_breaks = date_minor_breaks,
			  expand = expand
			)
		}else{
			px=geom_ts(df=plotdf, yl=2, yr=NULL, 
			yllab=ylab[[i-1]], yrlab=NULL, xlab=xlab,
			llist=2, plist=2, 
			llab=NULL, plab=NULL, alab=NULL, blab=NULL,
			ltype=NULL, pshape=NULL, 
			lsize=1, psize=1,
			lcc=c(cclist[i-1]), pcc=c(cclist[i-1]))+
			scale_x_datetime(
			  breaks = breaks,
			  date_breaks = date_breaks,
			  labels = labels,
			  date_labels = date_labels,
			  minor_breaks = minor_breaks,
			  date_minor_breaks = date_minor_breaks,
			  expand = expand
			)	
		}
		if(i!=ncol(df)){	
			px=px+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
		}
		eval(parse(text=paste0("p",i-1,"=px")))
		if(i!=2){
					cfig=paste0(cfig,"+p",i-1)
				}else{
					cfig=paste0("p",i-1)
				}	
	}

	p=eval(parse(text=paste0("(",cfig," & theme(legend.position = 'none', plot.margin = margin(t=1, r=5, b=", panelgap, ", l=5)))+ plot_layout(guides = 'collect',ncol = 1)")))
	
	return(p)
}