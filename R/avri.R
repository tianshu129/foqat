#' Calculate average of variation
#'
#' Calculates average of variation of time series.
#'
#' If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.
#'
#' @param df dataframe contains time series.
#' @param bkip break input of resampling, such as '1 hour'.
#' @param cycle cycle for average variation. By default, cycle is "1 day".
#' @param n number of items for each cycle. If n is setted, cycle will invalid.
#' @param colid column index of datetime in dataframe.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param na.rm logical value. Remove NA value or not?
#' @param wind logical value. if TRUE, please set coliwd, coliws.
#' @param coliws numeric value, colindex of wind speed in dataframe.
#' @param coliwd numeric value, colindex of wind direction (degree) in dataframe.
#' @return  dataframe for average variation.
#' @export
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom lubridate duration

avri<-function(df, bkip, cycles="1 day", n=NULL, colid = 1, st = st, et = et, na.rm = na.rm, wind = wind, coliws = 2, coliwd = 3){
  #time resampling
  rs_df <- trs(df, bkip, colid = 1, st = st, et = et, na.rm = na.rm, wind = FALSE, coliws = coliws, coliwd = coliwd)

  #get colnames of rs_df
  cona_df <- colnames(rs_df)

  #generate u, v
  if(wind == TRUE){
    rs_df$u<-sin(pi/180*rs_df[,3])*rs_df[,2]
    rs_df$v<-cos(pi/180*rs_df[,3])*rs_df[,2]
  }

  #claculate number for per_cycle
  if(is.null(n)){
    n=duration(cycle)/duration(bkip)
  }
  mod_list=seq(0,nrow(rs_df)-1,1)%%n

  #avearage
  results=aggregate(rs_df[,-1], by=list(cycle=mod_list), mean, na.rm = na.rm)
  results=data.frame(Time=rs_df[1:24,1],results)
  colnames(results)[1]<-"Timestamp Reference"



  #sd
  results_sd=aggregate(rs_df[,-1], by=list(cycle=mod_list), sd, na.rm = na.rm)
  results_sd=data.frame(Time=rs_df[1:24,1],results_sd)
  colnames(results_sd)[1]<-"Timestamp Reference"

  #calculate avearage of ws, wd
  if(wind == TRUE){
    datat=results
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$ws<-sqrt((datat$u)^2+(datat$v)^2)
    datat <- within(datat, {
      true_degree = ifelse(datat$v<0,datat$fake_degree+180,ifelse(datat$u<0,datat$fake_degree+360,datat$fake_degree))
    })
    datat <- datat[ ,-which(names(datat) %in% c("u", "v", "fake_degree"))]
    datat[ ,c(3,4)] <- datat[ ,c((length(datat)-1),length(datat))]
    datat <- datat[,-c((length(datat)-1),length(datat))]
    results=datat
  }

  #calculate sd of ws, wd
  if(wind == TRUE){
    datat=results_sd
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$ws<-sqrt((datat$u)^2+(datat$v)^2)
    datat <- within(datat, {
      true_degree = ifelse(datat$v<0,datat$fake_degree+180,ifelse(datat$u<0,datat$fake_degree+360,datat$fake_degree))
    })
    datat <- datat[ ,-which(names(datat) %in% c("u", "v", "fake_degree"))]
    datat[ ,c(3,4)] <- datat[ ,c((length(datat)-1),length(datat))]
    datat <- datat[,-c((length(datat)-1),length(datat))]
    results_sd=datat
  }

  #format average data
  results[,-c(1,2)]=lapply(results[,-c(1,2)], formatC, format = "e", digits = 2)

  #format sd data
  results_sd[,-c(1,2)]=lapply(results_sd[,-c(1,2)], formatC, format = "e", digits = 2)

  #rename average df
  colnames(results)[3:ncol(results)] <- cona_df[2:length(cona_df)]

  #rename sd df
  colnames(results_sd)[3:ncol(results_sd)] <- cona_df[2:length(cona_df)]

  #output
  df_average=results
  df_sd=results_sd
  results <- list(df_average = df_average, df_sd = df_sd)
  return(results)
}
