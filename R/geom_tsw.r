#' Plot time series
#'
#' Easy way to plot time series.
#' 
#' @param df dataframe contains time series.
#' @param coliws column index of wind speed. The default vaule is 2.
#' @param coliwd column index of wind direction. The default vaule is 3.
#' @param lsize size of line (wind speed). The default vaule is 0.8.
#' @param psize size of point (wind speed). The default vaule is NA.
#' @param msize size of mark (wind direction). The default vaule is 8.
#' @param mlabel label of mark (wind direction). The default vaule is "West wind".
#' @param mx adjust value for the x position of mark (wind direction). The default vaule is 0.05.
#' @param my adjust value for the y position of mark (wind direction). The default vaule is -0.1.
#' @param mwd direction of mark (wind direction). The default vaule is 270.
#'
#' @export
#' @examples
#' metds=trs(met, bkip="15 mins")
#' geom_tsw(metds, coliws=4, coliwd=5)
#' @importFrom ggplot2 ggplot geom_line geom_text scale_y_continuous scale_colour_viridis_c theme_bw

geom_tsw <- function(df, coliws = 2, coliwd = 3, lsize = 0.8, psize=NA, msize = 8, mlabel = "West wind", mx = 0.05, my = -0.1, mwd = 270){
	met_wind=df
	names(met_wind)[c(1, coliws, coliwd)]=c("Datetime", "WSfoqat", "WDfoqat")
	met_wind$WSfoqat=met_wind$WSfoqat#*0.01
	max_ws=mean(na.omit(met_wind$WSfoqat))
	#
  mdf=data.frame(Datetime=mx*(max(met_wind$Datetime)-min(met_wind$Datetime))+min(met_wind$Datetime), WDfoqat=mwd)
	#
	fixx=met_wind$Datetime[2]-met_wind$Datetime[1]
	#
	p=ggplot(data=met_wind) +
	geom_line(aes(x = Datetime, y = WSfoqat, color = WSfoqat), size=lsize)+
	geom_point(aes(x = Datetime, y = WSfoqat, color = WSfoqat), size=psize)+
    geom_text(aes(x = Datetime+fixx, y = -0.5*max_ws, angle=ifelse(is.na(WDfoqat), 0, 270-WDfoqat), label=ifelse(is.na(WDfoqat), "", "\U2192"), color = WSfoqat, vjust = 0, hjust = 0),size=msize)+
	scale_y_continuous(limits = c(-1*max_ws, NA), breaks=pretty(met_wind$WSfoqat, n = 4), name = expression("Wind speed (m/s)"), expand=c(0,0))+
	scale_colour_viridis_c(direction = 1)+
	geom_text(data=mdf, aes(x = Datetime, y = my*max_ws, angle=270-WDfoqat, label="\U2192", vjust = 0, hjust = 0),size=msize)+
	geom_text(data=mdf, aes(x = Datetime, y = my*max_ws, label=mlabel, vjust = -0.2, hjust = -0.5), size=0.6*msize)+	
	theme_bw()+
	scale_x_datetime(expand = c(0, 0))+ 
	labs(color='Wind speed') 
	return(p)
}