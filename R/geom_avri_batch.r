#' Plot the average variation in batch
#'
#' Easy way to plot the average variation in batch.
#'
#' @param df dataframe contains average variation value and their standard deviation.
#' @param ssd scale value for standard deviation. The default vaule is 1.
#' @param alpha the alpha value of ribbon. The default vaule is 0.5.
#' @param xlab text expression of x axis label. The default vaule is NULL. Need to set with ylab at the same time.
#' @param ylab text expression of y axis label. The default vaule is NULL. Need to set with xlab at the same time.
#' @param lcc colors of lines. The default vaule is NULL. Need to set with rff at the same time.
#' @param lsize size of lines. The default vaule is NULL.The default vaule is 1.
#' @param rff fill colors of ribbons. The default vaule is NULL. Need to set with lcc at the same time.
#' @param ncol number of figure columns in final plot layout. The default vaule is 2.
#' @param bquote logical value. Set to TRUE if you want to use bquote in labs (xlab and y lab). The default vaule is FALSE.
#'
#' @export
#' @examples
#' \dontrun{ 
#' #example 1
#' x=avri(aqi, bkip = "1 hour", mode = "recipes", value
#'  = "day", st = "2017-05-01 00:00:00")
#' geom_avri_batch(x)
#' #example 2
#' x=avri(aqi, bkip = "1 hour", mode = "recipes", value
#'  = "day", st = "2017-05-01 00:00:00")
#' lcc=c("#f5222d","#fa8c16","#52c41a","#1890ff","#722ed1")
#' rff=c("#ff7875","#ffc069","#95de64","#69c0ff","#b37feb")
#' xlab1=list(bquote(Time~""),bquote(Time~""),bquote(Time~""),
#' bquote(Time~""),bquote(Time~""))
#' ylab1=list(bquote(NO~" "~(ppbv)), bquote(NO[2]~" "~(ppbv)), 
#' bquote(CO~" "~(ppmv)), bquote(SO[2]~" "~(ppbv)), bquote(O[3]~" "~(ppbv)))
#' geom_avri_batch(x, alpha=0.6, xlab=xlab1, ylab=ylab1,
#'  lcc=lcc, rff=rff, bquote=TRUE)
#' #example 3
#' x=avri(aqi, bkip = "1 hour", mode = "recipes", value
#'  = "day", st = "2017-05-01 00:00:00")
#' xlab2=rep("Time",5)
#' ylab2=c("NO","NO2","CO","SO2","O3")
#' geom_avri_batch(x, alpha=0.6, xlab=xlab2, ylab=ylab2,
#'  lcc=lcc, rff=rff, bquote=FALSE)
#' }
#' @importFrom lubridate is.timepoint
#' @import patchwork 

geom_avri_batch<-function(df, ssd=1, alpha=0.5, xlab=NULL, ylab=NULL, lcc=NULL, lsize=1, rff=NULL, ncol=2, bquote=FALSE){	
	for(i in 2:((ncol(df)-1)/2+1)){
		if(length(xlab)==0&length(ylab)==0&length(lcc)==0&length(rff)==0){
			eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", lcc='#0050b3', lsize=",lsize, ", rff='#40a9ff')")))
			if(i!=2){
				cfig=paste0(cfig,"+p",i-1)
			}else{
				cfig=paste0("p",i-1)
			}
		}else if(length(xlab)==0&length(ylab)==0&length(lcc)!=0&length(rff)!=0){
			eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", lcc=lcc[i-1], lsize=", lsize, ", rff=rff[i-1])")))
			if(i!=2){
				cfig=paste0(cfig,"+p",i-1)
			}else{
				cfig=paste0("p",i-1)
			}
		}else if(length(xlab)!=0&length(ylab)!=0&length(lcc)==0&length(rff)==0){
			if(bquote==FALSE){
				eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", xlab=xlab[i-1], ylab=ylab[i-1], lcc='#0050b3', lsize=", lsize, ", rff='#40a9ff')")))
			}else{
				eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", xlab=xlab[[i-1]], ylab= ylab[[i-1]], lcc='#0050b3', lsize=", lsize, ", rff='#40a9ff')")))
			}
			if(i!=2){
				cfig=paste0(cfig,"+p",i-1)
			}else{
				cfig=paste0("p",i-1)
			}
		}else if(length(xlab)!=0&length(ylab)!=0&length(lcc)!=0&length(rff)!=0){
			if(bquote==FALSE){
				eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", xlab=xlab[i-1], ylab=ylab[i-1], lcc=lcc[i-1], lsize=", lsize, ", rff=rff[i-1])")))
			}else{
				eval(parse(text=paste0("p",i-1,"=geom_avri(df,cave=",i,", csd=",i+(ncol(df)-1)/2,", ssd=",ssd,", alpha=",alpha,", xlab=xlab[[i-1]], ylab=ylab[[i-1]], lcc=lcc[i-1], lsize=", lsize, ", rff=rff[i-1])")))
			}
			if(i!=2){
				cfig=paste0(cfig,"+p",i-1)
			}else{
				cfig=paste0("p",i-1)
			}			
		}
	}
	
	p=eval(parse(text=paste0("(",cfig," & theme(legend.position = 'none'))+ plot_layout(guides = 'collect',ncol = ",ncol,")")))
	
	return(p)
}