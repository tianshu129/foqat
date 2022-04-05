#' Plot the average variation
#'
#' Easy way to plot the average variation.
#'
#' @param df dataframe contains average variation value and their standard deviation.
#' @param cave column index of average variation. The default vaule is 2.
#' @param csd column index of standard deviation. The default vaule is 3.
#' @param ssd scale value for standard deviation. The default vaule is 1.
#' @param alpha the alpha value of ribbon. The default vaule is 0.5.
#' @param xlab text expression of x axis label. The default vaule is NULL.
#' @param ylab text expression of y axis label. The default vaule is NULL.
#' @param lcc color of line. The default vaule is NULL. 
#' @param lsize size of line. The default vaule is NULL.The default vaule is 1.
#' @param rff fill color of ribbon. The default vaule is NULL.
#'
#' @export
#' @examples
#' \dontrun{ 
#' x=avri(aqi, bkip = "1 hour", mode = "recipes", value
#'  = "day", st = "2017-05-01 00:00:00")
#' geom_avri(x,cave=6, csd=11, alpha=0.5, lcc="#0050b3",
#'  rff="#40a9ff", xlab="Time",ylab=bquote(O[3]~" "~(ppbv)))
#' }
#' @importFrom lubridate is.timepoint

geom_avri<-function(df,cave=2, csd=3, ssd=1, alpha=0.5, xlab=NULL, ylab=NULL, lcc=NULL, lsize=1, rff=NULL){
	Datetime=df[,1]
	conc=df[,cave]
	sdc=df[,csd]*ssd
	gtype=rep("ha",nrow(df))
	conc=as.numeric(conc)
	sdc=as.numeric(sdc)
	dv=data.frame(Datetime=Datetime, conc=conc, ymin=conc-sdc,ymax=conc+sdc,gtype=gtype)
	p=ggplot(data=dv, aes(x=Datetime, y=conc, ymin=ymin, ymax=ymax , fill=gtype, linetype=gtype))
	if(length(rff)!=0){
		p = p + geom_ribbon(alpha=alpha, fill=rff)
	}else{
		p = p + geom_ribbon(alpha=alpha)
	}
	
	if(length(lcc)!=0){
		p = p + geom_line(color=lcc, size=lsize) 
	}else{
		p = p + geom_line(size=lsize)
	}
	
	 
	#加y标题#################################
	if(length(ylab)!=0){
		p = p + ylab(ylab)
	}else{
		p = p + ylab(names(df)[cave])
	}

	#加x标题#################################
	if(length(xlab)!=0){
		p = p + xlab(xlab)
	}else{
		p = p + xlab(names(df)[1])
	}
	
	if(is.timepoint(dv[1,1])){
		p1=p+theme_bw() + theme(legend.position = "none")+
		scale_x_datetime(expand = c(0, 0)) 
	}else{
		p1=p+theme_bw() + theme(legend.position = "none")+
		scale_x_continuous(expand = c(0, 0)) 
	}
	return(p1)
}