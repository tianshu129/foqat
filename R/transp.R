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
#' @param logy logical. The default vaule is TRUE. If TRUE, plot the data with log y axis.
#' @param ybk numeric vector, breaks of y axis of plot.
#' @param nlmt numeric value, uplimit of dNdlogdp colorscales of plot.
#' @param colsz numeric value, size of columns in plot.
#' @return a dataframe. If the input is a table, the output is a list, and if the input is a list, the output is a table. 
#' @export
#' @importFrom reshape2 dcast melt
#' @importFrom ggplot2 ggplot geom_point scale_y_log10 scale_fill_gradientn guide_colorbar labs
#' @importFrom ggprism annotation_logticks annotation_ticks
#' @importFrom graphics plot
#' @importFrom grDevices colorRampPalette

transp <- function(df, prplot=TRUE, logy=TRUE, ybk=c(10,100,500,1000), nlmt=20000, colsz=10){
	temp_name=names(df)[1]
	names(df)[1]="Datetime"
	if(ncol(df)==3){
		listp=df
		tablep=eval(parse(text = paste(c("dcast(df,", names(df)[1], " ~ ", names(df)[2], ")"),collapse = "")))
	}else{
		listp=eval(parse(text = paste0(c("melt(df, id.vars = '", names(df)[1] ,"')"),collapse = "")))
		listp[,2]=gsub("[^0123456789.]", "", listp[,2])
		listp[,2]=as.numeric(listp[,2])
		tablep=df
	}
	
	#plot
	if(prplot==TRUE){
		fm2= theme_bw()
		afont_size=13
		family="Helvetica"
		fm3= theme(axis.text.x = element_text(size=afont_size,family=family), axis.text.y = element_text(size=afont_size,family=family), text = element_text(size=afont_size,family=family), legend.text = element_text(size=afont_size,family=family), panel.border = element_rect(fill=NA,color="black", size=0.4, linetype="solid"), axis.line = element_line(colour = "black", size = 0.4),legend.box.margin=margin(-10,-10,-10,-10))

		fm=function(p){
			p=p+fm2+fm3
			return(p)
		}
		df=listp
		colors <- colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(500)
		if(logy==TRUE){		
			p=eval(parse(text = paste(c("ggplot() + geom_point(data=df, aes(x=", colnames(df)[1], ", y=", colnames(df)[2], ", fill=ifelse(", colnames(df)[3], "<nlmt,", colnames(df)[3], ",ifelse(", colnames(df)[3], ">=nlmt,nlmt,", colnames(df)[3], "))),shape=22, color='transparent',size=colsz) + scale_y_log10(breaks = ybk, labels = ybk, expand = c(0, 0)) + scale_x_datetime(expand = c(0, 0)) + scale_fill_gradientn(limits = c(0,nlmt),colors= colors,name='dn/dlogdp',guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),na.value='transparent') + annotation_logticks(sides = 'lr') + labs(x = 'Datetime', y = 'Midrange')"),collapse = "")))
			p=fm(p)
		}else{
			p=eval(parse(text = paste(c("ggplot() + geom_point(data=df, aes(x=", colnames(df)[1], ", y=", colnames(df)[2], ", fill=ifelse(", colnames(df)[3], "<nlmt,", colnames(df)[3], ",ifelse(", colnames(df)[3], ">=nlmt,nlmt,", colnames(df)[3], "))),shape=22, color='transparent',size=colsz) + scale_y_continuous(breaks = ybk, labels = ybk, expand = c(0, 0)) + scale_x_datetime(expand = c(0, 0)) + scale_fill_gradientn(limits = c(0,nlmt),colors= colors,name='dn/dlogdp',guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),na.value='transparent') + annotation_ticks(sides = 'lr') + labs(x = 'Datetime', y = 'Midrange')"),collapse = "")))
			p=fm(p)
		}
		plot(p)
	}
  
	#output
	if(ncol(df)==3){
		names(tablep)[1]=temp_name
		results <- tablep
	}else{
		names(listp)[1]=temp_name
		results <- listp
	}
	return(results)	
}
