#' Plot the time series of particle size distribution.
#'
#' Plot the time series of particle size distribution.
#'
#' @param df dataframe of particle size data: the first column of input is datetime; the other columns are number concentration (N, unit: #/cm3) or log number concentration (dN/dlogdp, unit: #/cm3) for each particle size channel. Column names of the other columns are the middle particle size for each particle size channel.  
#' @param labxyl vector, Set the title of x axis, y axis, legend. The default vaule is NULL. Bquote grammer is accepted.
#' @param logy logical. Plot the data with log y axis. The default vaule is TRUE.
#' @param ybk numeric vector, breaks of y axis.
#' @param nlmt numeric value, range of particle number for colorscales of plot.
#' @param csbk numeric vector, breaks of color bar.
#' @param trans character string, "identity" or "log10". transformation of color bar breaks.
#' @param colsz numeric value, size of columns in plot.
#' @param fsz font size in plot.
#' @param ff font family in plot.
#' @param lsz line size of panel border and axis in plot.
#' @param tkl tick length in plot.
#' @return a plot for the time series of particle size distribution. \cr
#'
#' @export
#' @examples
#' \dontrun{ 
#' dn_table = read.delim(system.file("extdata", "smps.txt", package = "foqat"),
#' check.names = FALSE)
#' dn1_table=dn_table[,c(1,5:148)]
#' dn1_table[,1]=as.POSIXct(dn1_table[,1], format="%m/%d/%Y %H:%M:%S", tz="GMT")
#' geom_psd(dn1_table,fsz=10)
#' }
#' @import ggplot2 
#' @importFrom scales rescale pretty_breaks
#' @importFrom grDevices colorRampPalette

geom_psd=function(df, labxyl=NULL, logy=TRUE, ybk=NULL, nlmt=NULL, csbk=pretty_breaks(4), trans="identity", colsz=1, fsz=13, ff="TT Arial", lsz=0.4, tkl=0.2){
	#trans from table to list
	df=transp(df)
	
	#save col name
	temp_name2=names(df)
	names(df)=c("Datetime","Midrange","dN_dlogdp")
	
	#set nlmt
	if(is.null(nlmt)&trans!="log10"){nlmt=c(min(df[,3],na.rm = TRUE),max(df[,3],na.rm = TRUE))}
	
	if(is.null(nlmt)&trans=="log10"){nlmt=c(1,max(df[,3],na.rm = TRUE))}
	
	#replace by nlmt
	if(!is.null(nlmt)){df[,3]=ifelse(df[,3]<=nlmt[1], nlmt[1], ifelse(df[,3]>=nlmt[2],nlmt[2],df[,3]))}

	#basic plot
	p=eval(parse(text = paste(c("ggplot() + geom_point(data=df, aes(x=", colnames(df)[1], ", y=", colnames(df)[2], ", fill=", colnames(df)[3], "),shape=22, color='transparent',size=colsz)"),collapse = "")))

	#scale_y
	if(logy==TRUE){
		if(is.null(ybk)){
			p=p+scale_y_log10(expand = c(0, 0))
		}else{
			p=p+scale_y_log10(breaks = ybk, labels = ybk, expand = c(0, 0))
		}
	}else{
		if(is.null(ybk)){
			p=p+scale_y_continuous(expand = c(0, 0))
		}else{
			p=p+scale_y_continuous(breaks = ybk, labels = ybk, expand = c(0, 0))
		}
	}

	#expand_x
	if(is.numeric(df[,1])|is.character(df[,1])){
		p=p+scale_x_continuous(expand = c(0, 0))
	}else{
		p=p+scale_x_datetime(expand = c(0, 0))
	}

	#scale_fill+#labs
	if(is.null(nlmt)){
	crr=1
		#colors
		colors <- c(colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(1000))
	}else{
	crr=1.1
		#colors
		colors <- c(colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(900),colorRampPalette(c("red"))(100))
	}
	
	if(is.null(labxyl)){
		labx=temp_name2[1]
		laby=temp_name2[2]
		labl=temp_name2[3]
	}else if(!is.list(labxyl)){
		labx=labxyl[1]
		laby=labxyl[2]
		labl=labxyl[3]
	}else{
		labx=labxyl[[1]]
		laby=labxyl[[2]]
		labl=labxyl[[3]]
	}	
		
	p=p+scale_fill_gradientn(name=labl, breaks=csbk, trans=trans, limits = c(nlmt[1],nlmt[2]*crr), values= rescale(nlmt), colors= colors,guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black',frame.linewidth=lsz),na.value='transparent')+labs(x = labx, y = laby)
	
	#annotation_logticks
	p=p+annotation_logticks(sides = 'lr')
	
	p=fm(p, fsz=fsz, ff=ff, lsz=lsz, tkl=tkl)

	return(p)
}
