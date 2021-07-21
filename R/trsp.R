#' Resample time series of particle number concentration
#'
#' Resamples time series, and returns complete time series of of particle number concentration with new time resolution. 
#' The first column of input is datetime. The second column of input is for middle ranges of channels. 
#' The third column of input is for particle number concentration of each channel at each timepoint.
#'
#'
#' @param df dataframe of time series.  The first column of input is datetime. 
#' The second column of input is for middle ranges of channels. 
#' The third column of input is for particle number concentration of each channel at each timepoint.
#' @param bkip new resolution breaking input of time series, such as '1 hour'.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param na.rm logical value. Remove NA value or not?
#' @param cpms logical value. Compensate the insufficient amount of the millisecond bit for datetime column.
#' @param ybk numeric vector, log breaks of y axis of plot.
#' @param nlmt numeric value, uplimit of dNdlogdp colorscales of plot.
#' @param colsz numeric value, size of columns in plot.
#' @return a list with 1 dataframe and 1 plot. The dataframe contains a time series with a new time resolution.
#' @export
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom ggplot2 ggplot geom_point scale_y_log10 scale_fill_gradientn guide_colorbar annotation_logticks labs
#' @importFrom graphics plot
#' @importFrom grDevices colorRampPalette

trsp <- function(df, bkip, st=NULL, et=NULL, na.rm = TRUE, cpms=TRUE, ybk=c(10,100,500,1000), nlmt=20000, colsz=10){
  
  #In case df is not a dataframe.
  df <- data.frame(df,stringsAsFactors = FALSE)
  ori_df=df
  #get colnames of df
  cona_df <- colnames(df)
  #colnames(df)=c("Datetime", "Midrange", "dN_dlogdp")
  
  #get time zone of datetime
  tzlc=attr(df[,1],"tzone")
  
  #compensate the insufficient amount of the millisecond bit for datetime column
  if(cpms==TRUE){
	  msid=which((as.numeric(format(df[,1], "%OS1"))-as.numeric(format(df[,1], "%OS")))!=0)
	  df[msid,1]=df[msid,1]-(as.numeric(format(df[msid,1], "%OS1"))-as.numeric(format(df[msid,1], "%OS")))+1
  }
  

  #aggregate, seq need space
  if(!grepl("\\s", bkip)){
    #if not space in bkip
    bkip <- gsub("([0-9])([a-z])", "\\1 \\2", bkip)
  }
  #set format for datetime
  df[,1] = as.POSIXct(df[,1], '%Y-%m-%d %H:%M', tz = tzlc)
  #input st, et?
  if(is.null(st)){
    #if not input st, set first timestamp as st
    st = as.POSIXct(df[1,1], '%Y-%m-%d %H:%M', tz = tzlc)
  }else{
    #if input st, cut df by st
    st = as.POSIXct(st, format = "%Y-%m-%d %H:%M", tz = tzlc)
    ##if st not exist in df, insert st
    if(st != df[min(which(df[,1] >= st)),1]){
      k=min(which(df[,1] >= st))
      df[seq(k+1,nrow(df)+1),] <- df[seq(k,nrow(df)),]
      df[k,1] <- as.POSIXct(st, tz = tzlc)
      df[k,2:ncol(df)] <- c(rep(NA, ncol(df)-1))
    }
    df<-df[df[,1]>=st,]
  }
  if(is.null(et)){
    #if not input et, set last timestamp as et
    et<-as.POSIXct(df[length(df[,1]),1],'%Y-%m-%d %H:%M',tz= tzlc)
  }else{
    #if input et, cut df by et
    et=as.POSIXct(et,format="%Y-%m-%d %H:%M",tz= tzlc)
    #not need to insert et, just need to trunck by et (">=").
    df <- df[df[,1] <= et,]
  }
  eval(parse(text = paste(c("datat <- aggregate(df[,3], list(", colnames(df)[1], " = cut(df[,1], breaks = bkip),", colnames(df)[2], "=df[,2]), mean, na.rm = na.rm)"),collapse = "")))

  #convert ts according to type of bkip
  bkip_str = gsub("[^a-zA-Z]", "", bkip)
  bkip_str = tolower(bkip_str)
  bkip_str = gsub("[s]", "", bkip_str)
  if(bkip_str == "sec"|bkip_str == "min"|bkip_str == "hour"){
	datat[,1] = as.POSIXct(datat[,1], "%Y-%m-%d %H:%M", tz = tzlc)
  }else{
	datat[,1] = as.Date(datat[,1])
  }


  #gnerate timeseries according to type of bkip
  bkip_str = gsub("[^a-zA-Z]", "", bkip)
  bkip_str = tolower(bkip_str)
  bkip_str = gsub("[s]", "", bkip_str)
  if(bkip_str == "sec"|bkip_str == "min"|bkip_str == "hour"){
	ts <- seq.POSIXt(st, et, by = bkip)
	df=expand.grid(ts,unique(ori_df[,2]))
	df=data.frame(df)
	names(df)=c(names(ori_df)[1:2])
  }else{
	st <- as.Date(st)
	et <- as.Date(et)
	ts <- seq(st, et, by = bkip)
	df=expand.grid(ts,unique(ori_df[,2]))
	df=data.frame(df)
	names(df)=c(names(ori_df)[1:2])
  }

  #complete timestamp
  eval(parse(text = paste(c("df <- full_join(df, datat,by = c('", colnames(datat)[1], "' = '", colnames(datat)[1], "', '", colnames(datat)[2], "' = '", colnames(datat)[2], "'))"),collapse = "")))

  #In case first row is NaN
  df[,-1][df[,-1] == "NaN"] <- NA

  #rename df
  colnames(df) <- cona_df
  
  #set format for datetime
  df[,1] = as.POSIXct(df[,1], '%Y-%m-%d %H:%M', tz = tzlc)
  df=df[order( df[,1]),]
  
  #plot
  colors <- colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(500)
  p=eval(parse(text = paste(c("ggplot() + geom_point(data=df, aes(x=", colnames(df)[1], ", y=", colnames(df)[2], ", fill=ifelse(", colnames(df)[3], "<nlmt,", colnames(df)[3], ",ifelse(", colnames(df)[3], ">=nlmt,nlmt,", colnames(df)[3], "))),shape=22, color='transparent',size=colsz) + scale_y_log10(breaks = ybk,labels = ybk) + scale_fill_gradientn(limits = c(0,nlmt),colors= colors,name='dn/dlogdp',guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),na.value='transparent') + annotation_logticks(sides = 'lr') + labs(x = 'Datetime', y = 'Midrange')"),collapse = "")))
  plot(p)
  
  results <- list(results=df, img=p)
  #output
  return(results)
}