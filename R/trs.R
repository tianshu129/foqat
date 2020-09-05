#' Resample time series
#'
#' Resamples time series, and returns complete time series with new time resolution.
#'   (wind data is acceptable)
#'
#' If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.
#'
#' @param df dataframe of time series.
#' @param bkip new resolution breaking input of time series, such as '1 hour'.
#' @param colid column index for date-time. The default value is 1.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param na.rm logical value. Remove NA value or not?
#' @param wind logical value. if TRUE, please set coliwd, coliws.
#' @param coliws numeric value, column index of wind speed in dataframe.
#' @param coliwd numeric value, column index of wind direction (degree) in dataframe.
#' @param cpms logical value. Compensate the insufficient amount of the millisecond bit for datetime column.
#' @return a dataframe which contains a time series with a new time resolution.
#' @export
#' @examples
#' trs(met, bkip = "1 hour", st = "2017-05-01 00:00:00", wind = TRUE, coliws = 4, coliwd = 5)
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom lubridate duration

trs <- function(df, bkip, colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3, cpms=TRUE){
  options(warn = -1)
  #move datetime to first column
  if(colid != 1){
    df[,c(1,colid)] = df[,c(colid,1)]
    colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
  }

  #In case df is not a dataframe.
  df <- data.frame(df,stringsAsFactors = FALSE)

  #get colnames of df
  cona_df <- colnames(df)
  
  #get time zone of datetime
  tzlc=attr(df[,1],"tzone")
  
  #compensate the insufficient amount of the millisecond bit for datetime column
  if(cpms==TRUE){
	  msid=which((as.numeric(format(df[,1], "%OS1"))-as.numeric(format(df[,1], "%OS")))!=0)
	  df[msid,1]=df[msid,1]-(as.numeric(format(df[msid,1], "%OS1"))-as.numeric(format(df[msid,1], "%OS")))+1
  }
  
  #if wind mode TURE, generate u, v
  if(wind == TRUE){
    df$u<-sin(pi/180*df[,coliwd])*df[,coliws]
    df$v<-cos(pi/180*df[,coliwd])*df[,coliws]
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
    et<-as.POSIXct(as.character(df[length(df[,1]),1]),'%Y-%m-%d %H:%M',tz= tzlc)
  }else{
    #if input et, cut df by et
    et=as.POSIXct(et,format="%Y-%m-%d %H:%M",tz= tzlc)
    #not need to insert et, just need to trunck by et (">=").
    df <- df[df[,1] <= et,]
  }
  eval(parse(text = paste(c("datat <- aggregate(df[,-1], list(", colnames(df)[1], " = cut(df[,1], breaks = bkip)), mean, na.rm = na.rm)"),collapse = "")))

  #convert ts according to type of bkip
  bkip_str = gsub("[^a-zA-Z]", "", bkip)
  bkip_str = tolower(bkip_str)
  bkip_str = gsub("[s]", "", bkip_str)
  if(bkip_str == "sec"|bkip_str == "min"|bkip_str == "hour"){
	datat[,1] = as.POSIXct(datat[,1], "%Y-%m-%d %H:%M", tz = tzlc)
  }else{
	datat[,1] = as.Date(datat[,1])
  }

  if(wind == TRUE){
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$ws<-sqrt((datat$u)^2+(datat$v)^2)
    datat <- within(datat, {
      true_degree = ifelse(datat$v<0,datat$fake_degree+180,ifelse(datat$u<0,datat$fake_degree+360,datat$fake_degree))
    })
    datat <- datat[ ,-which(names(datat) %in% c("u", "v", "fake_degree"))]
    datat[ ,c(coliws,coliwd)] <- datat[ ,c((length(datat)-1),length(datat))]
    datat <- datat[,-c((length(datat)-1),length(datat))]
  }

  #gnerate timeseries according to type of bkip
  bkip_str = gsub("[^a-zA-Z]", "", bkip)
  bkip_str = tolower(bkip_str)
  bkip_str = gsub("[s]", "", bkip_str)
  if(bkip_str == "sec"|bkip_str == "min"|bkip_str == "hour"){
	ts <- seq.POSIXt(st, et, by = bkip)
	eval(parse(text = paste(c("df <- data.frame(", colnames(datat)[1], " = ts)"),collapse = "")))
  }else{
	st <- as.Date(st)
	et <- as.Date(et)
	ts <- seq(st, et, by = bkip)
	eval(parse(text = paste(c("df <- data.frame(", colnames(datat)[1], " = ts)"),collapse = "")))
  }

  #complete timestamp
  df <- full_join(df, datat)

  #In case first row is NaN
  df[,-1][df[,-1] == "NaN"] <- NA

  #rename df
  colnames(df) <- cona_df
  
  #set format for datetime
  df[,1] = as.POSIXct(df[,1], '%Y-%m-%d %H:%M', tz = tzlc)
  
  #output
  options(warn = 0)
  return(df)
}