#' Convert the format of particle size data
#'
#' Converting the format of particle size data. There are 2 types of particle size data: table and list.
#' For table format: the first column of input is datetime; the other column is the number concentration of each particle size channel, column name is the middle particle size of the particle size channel.  
#' For list format: the first column of input is datetime. The second column of input is for middle ranges of channels. 
#' The third column of input is for particle number concentration of each channel at each timepoint.
#'
#'
#' @param df dataframe of particle size data: a table or a list.
#' @param prplot logical. The default vaule is TRUE. If TRUE, plot the data.
#' @param ybk numeric vector, log breaks of y axis of plot.
#' @param nlmt numeric value, uplimit of dNdlogdp colorscales of plot.
#' @param colsz numeric value, size of columns in plot.
#' @return a dataframe. If the input is a table, the output is a list, and if the input is a list, the output is a table. 
#' @export
#' @importFrom reshape2 dcast melt
#' @importFrom ggplot2 ggplot geom_point scale_y_log10 scale_fill_gradientn guide_colorbar annotation_logticks labs
#' @importFrom graphics plot
#' @importFrom grDevices colorRampPalette

transp <- function(df, prplot=TRUE, ybk=c(10,100,500,1000), nlmt=20000, colsz=10){
	if(ncol(df)==3){
		listp=df
		tablep=eval(parse(text = paste(c("dcast(df,", names(df)[1], " ~ ", names(df)[2], ")"),collapse = "")))
	}else{
		listp=eval(parse(text = paste(c("melt(df, id.vars = c(", names(df)[1] , names(df)[2] ,") , measure.vars = 'dN_dlogdp')"),collapse = "")))
		listp[,2]=gsub("[^0123456789.]", "", listp[,2])
		listp[,2]=as.numeric(listp[,2])
		tablep=df
	}
	
	#plot
	if(prplot==TRUE){
		df=listp
		colors <- colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(500)
		p=eval(parse(text = paste(c("ggplot() + geom_point(data=df, aes(x=", colnames(df)[1], ", y=", colnames(df)[2], ", fill=ifelse(", colnames(df)[3], "<nlmt,", colnames(df)[3], ",ifelse(", colnames(df)[3], ">=nlmt,nlmt,", colnames(df)[3], "))),shape=22, color='transparent',size=colsz) + scale_y_log10(breaks = ybk,labels = ybk) + scale_fill_gradientn(limits = c(0,nlmt),colors= colors,name='dn/dlogdp',guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),na.value='transparent') + annotation_logticks(sides = 'lr') + labs(x = 'Datetime', y = 'Midrange')"),collapse = "")))
		plot(p)
	}
  
	#output
	if(ncol(df)==3){
		results <- tablep
	}else{
		results <- listp
	}
	return(results)	
}