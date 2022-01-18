#' Resample time series for summary statistics
#'
#' Resamples time series for summary statistics, and returns complete time series with new time resolution.
#'   (wind data is acceptable)
#'
#' If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.
#'
#' @param df dataframe of time series.
#' @param bkip new resolution breaking input of time series, such as '1 hour'.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param fun  a function to compute the summary statistics which can be applied to all data subsets: 'sum', 'mean', 'median', 'min', 'max', 'sd' and 'quantile'.
#' @param probs numeric vector of probabilities with values in \([0,1]\).
#' @param na.rm logical value. Remove NA value or not?
#' @param wind logical value. if TRUE, please set coliwd, coliws.
#' @param coliws numeric value, column index of wind speed in dataframe.
#' @param coliwd numeric value, column index of wind direction (degree) in dataframe.
#' @param cpms logical value. Compensate the insufficient amount of the millisecond bit for datetime column.
#' @return a dataframe which contains a time series for summary statistics with a new time resolution.
#' @export
#' @examples
#' trs(met, bkip = "1 hour", st = "2017-05-01 00:00:00", wind = TRUE, coliws = 4, coliwd = 5)
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom lubridate duration

trs <- function(df, bkip, st = NULL, et = NULL, fun = 'mean', probs=0.5, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3, cpms=TRUE){
  
  #remove rows showing NA in datatime
  df=df[!is.na(df[,1]),]

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
  df[,1] = as.POSIXct(df[,1], tz = tzlc)
  #input st, et?
  if(is.null(st)){
    #if not input st, set first timestamp as st
    st = as.POSIXct(df[1,1], tz = tzlc) #"%Y-%m-%d %H:%M:%S"
  }else{
    #if input st, cut df by st
    st = as.POSIXct(st, tz = tzlc) #"%Y-%m-%d %H:%M:%S"
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
    et<-as.POSIXct(df[length(df[,1]),1], tz = tzlc) #"%Y-%m-%d %H:%M:%S"
  }else{
    #if input et, cut df by et
    et=as.POSIXct(et, tz = tzlc) #"%Y-%m-%d %H:%M:%S"
    #not need to insert et, just need to trunck by et (">=").
    df <- df[df[,1] <= et,]
  }
  if(fun!="quantile"){
	eval(parse(text = paste(c("datat <- aggregate(df[,-1], list(", colnames(df)[1], " = cut(df[,1], breaks = bkip)), FUN=", fun, ", na.rm = na.rm)"),collapse = "")))
  }else{
	eval(parse(text = paste(c("datat <- aggregate(df[,-1], list(", colnames(df)[1], " = cut(df[,1], breaks = bkip)), FUN = 'quantile', probs=", probs, ", na.rm = na.rm)"),collapse = "")))
  }
  #convert ts according to type of bkip
  bkip_str = gsub("[^a-zA-Z]", "", bkip)
  bkip_str = tolower(bkip_str)
  bkip_str = gsub("[s]", "", bkip_str)
  if(bkip_str == ""|bkip_str == "ec"|bkip_str == "min"|bkip_str == "hour"){
	datat[,1] = as.POSIXct(datat[,1], tz = tzlc) #"%Y-%m-%d %H:%M:%S"
  }else{
	datat[,1] = as.Date(datat[,1])
  }

  if(wind == TRUE){
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$temp_ws<-sqrt((datat$u)^2+(datat$v)^2)
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
  if(bkip_str == ""|bkip_str == "ec"|bkip_str == "min"|bkip_str == "hour"){
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
  df[,1] = as.POSIXct(df[,1], tz = tzlc) #"%Y-%m-%d %H:%M:%S"
  
  #output
  return(df)
}
