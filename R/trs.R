#' Resampe time series
#'
#' Resamples time series, and returns complete time series with new time resolution.
#'
#' If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.
#'
#' @param df dataframe contains time series.
#' @param coli column index of datetime in dataframe.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param bkip break input of resampling, such as '1 hour'.
#' @param na.rm logical value. Remove NA value or not?
#' @param wind logical value. if TRUE, please set coliwd, coliws.
#' @param coliwd numeric value, colindex of wind direction (degree) in dataframe.
#' @param coliws numeric value, colindex of wind speed in dataframe.
#' @return  dataframe with new resolution.
#' @export
#' @importFrom dplyr full_join
#' @importFrom stats aggregate

trs <- function(df, bkip, coli = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliwd = 2, coliws = 3){
  options(warn = -1)
  #move datetime to first column
  if(coli != 1){ori_df[,c(1, coli)] = ori_df[, c(coli,1)]}

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
  df[,1] = as.POSIXct(df[,1], '%Y-%m-%d %H:%M', tz = "GMT")
  #input st, et?
  if(is.null(st)){
    #if not input st, set first timestamp as st
    st = as.POSIXct(df[1,1], '%Y-%m-%d %H:%M', tz = "GMT")
  }else{
    #if input st, cut df by st
    st = as.POSIXct(st, format = "%Y-%m-%d %H:%M", tz = "GMT")
    ##if st not exist in df, insert st
    if(st != df[min(which(df[,1] >= st)),1]){
      k=min(which(df[,1] >= st))
      df[seq(k+1,nrow(df)+1),] <- df[seq(k,nrow(df)),]
      df[k,1] <- as.POSIXct(st,tz="GMT")
      df[k,2:ncol(df)] <- c(rep(NA, ncol(df)-1))
    }
    df<-df[df[,1]>=st,]
  }
  if(is.null(et)){
    #if not input et, set last timestamp as et
    et<-as.POSIXct(as.character(df[length(df[,1]),1]),'%Y-%m-%d %H:%M')
  }else{
    #if input et, cut df by et
    et=as.POSIXct(et,format="%Y-%m-%d %H:%M",tz= "GMT")
    ##if et not exist in df, insert et
    if(et != df[max(which(df[,1] <= et)),1]){
      k=max(which(df[,1] <= et))
      df[seq(k+1,nrow(df)+1),] <- df[seq(k,nrow(df)),]
      df[k,1] <- as.POSIXct(et,tz="GMT")
      df[k,2:ncol(df)] <- c(rep(NA, ncol(df)-1))
    }
    df <- df[df[,1] <= et,]
  }
  eval(parse(text = paste(c("datat <- aggregate(df[,-1], list(", colnames(df)[1], " = cut(df[,1], breaks = bkip)), mean, na.rm = na.rm)"),collapse = "")))
  datat[,1] = as.POSIXct(datat[,1], "%Y-%m-%d %H:%M", tz = "GMT")

  if(wind == TRUE){
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$ws<-sqrt((datat$u)^2+(datat$v)^2)
    datat <- within(datat, {
      true_degree = ifelse(datat$v<0,datat$fake_degree+180,ifelse(datat$u<0,datat$fake_degree+360,datat$fake_degree))
    })
    datat <- datat[ ,-c(coliws, coliwd)]
    datat <- datat[ ,-which(names(datat) %in% c("u", "v", "fake_degree"))]
    colnames(datat)[(length(datat)-1):length(datat)]<-c("ws", "wd")
  }

  #gnerate timeseries
  st <- as.POSIXct(df[1,1], '%Y-%m-%d %H:%M', tz="GMT")
  et <- as.POSIXct(df[length(datat[,1]),1], '%Y-%m-%d %H:%M', tz="GMT")
  ts <- seq.POSIXt(st, et, by = bkip)
  eval(parse(text = paste(c("df <- data.frame(", colnames(datat)[1], " = ts)"),collapse = "")))
  #complete timestamp
  df <- full_join(df, datat)
  options(warn = 0)
  return(df)
}
