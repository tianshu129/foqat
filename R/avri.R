#' Calculate average of variation
#'
#' Calculates average of variation of time series.
#'
#' If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.
#'
#' @param df dataframe contains time series.
#' @param bkip break input of resampling, such as '1 hour'.
#' @param mode for calculating cycles: "recipes", "ncycle", "custom".
#' "recipes" means using internal setting for calculation.
#' "ncycle" means setting number of items for per cycle.
#' "custom" means using 1 column in dataframe as a list of grouping elements for calculation.
#' @param value for mode. Possible values for "recipes" are "day", "week", "month", year".
#' "day" equals to 24 (hours) values in 1 day.
#' "week" equals to 7 (days) values in 1 week.
#' "month" equals to 31 (days) values in 1 month.
#' "year" equals to 12 (months) values in 1 year.
#' values for "ncycle" is a number representing number of items in per cycle.
#' values for "custom" is a number representing column index in dataframe.
#' @param colid column index of datetime in dataframe.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param na.rm logical value. Remove NA value or not?
#' @param digits numeric value, digits for result dataframe.
#' @param wind logical value. if TRUE, please set coliwd, coliws.
#' @param coliws numeric value, colindex of wind speed in dataframe.
#' @param coliwd numeric value, colindex of wind direction (degree) in dataframe.
#' @return  dataframe for average variation.
#' @export
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom lubridate duration

avri<-function(df, bkip, mode = "recipes", value = "day", colid = 1, st = NULL, et = NULL, na.rm = TRUE, digits = 2, wind = FALSE, coliws = 2, coliwd = 3){

  #time resampling
  rs_df <- trs(df, bkip, colid = 1, st = st, et = et, na.rm = na.rm, wind = wind, coliws = coliws, coliwd = coliwd)

  #get colnames of rs_df
  cona_df <- colnames(rs_df)

  #generate u, v
  if(wind == TRUE){
    rs_df$u<-sin(pi/180*rs_df[,3])*rs_df[,2]
    rs_df$v<-cos(pi/180*rs_df[,3])*rs_df[,2]
  }

  #mode
  #mode recipes custom ncycle
  #value day week month year
  if(mode=="recipes"){
    if(value=="day"){
      #24 hour in 1 day
      mod_list=hour(rs_df[,1])
    }else if(value=="week"){
      #7 days in 1 week
      mod_list=weekdays(rs_df[,1])
    }else if(value=="month"){
      #31 days in 1 month
      mod_list=day(rs_df[,1])
    }else if(value=="year"){
      #12 month in 1 year
      mod_list=month(rs_df[,1])
    }
  }else if(mode=="ncycle"){
    mod_list=seq(0,nrow(rs_df)-1,1)%%value
  }else if(mode=="custom"){
    mod_list=rs_df[,value]
  }

  #avearage
  results=aggregate(rs_df[,-1], by=list(cycle=mod_list), mean, na.rm = na.rm)

  #sd
  results_sd=aggregate(rs_df[,-1], by=list(cycle=mod_list), sd, na.rm = na.rm)

  #calculate avearage of ws, wd
  if(wind == TRUE){
    datat=results
    datat$fake_degree<-(atan(datat$u/datat$v)/pi*180)
    datat$ws<-sqrt((datat$u)^2+(datat$v)^2)
    datat <- within(datat, {
      true_degree = ifelse(datat$v<0,datat$fake_degree+180,ifelse(datat$u<0,datat$fake_degree+360,datat$fake_degree))
    })
    datat <- datat[ ,-which(names(datat) %in% c("u", "v", "fake_degree"))]
    datat[ ,c(2,3)] <- datat[ ,c((length(datat)-1),length(datat))]
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
    datat[ ,c(2,3)] <- datat[ ,c((length(datat)-1),length(datat))]
    datat <- datat[,-c((length(datat)-1),length(datat))]
    results_sd=datat
  }

  #format average data
  results[,-1]=lapply(results[,-1], formatC, format = "e", digits = digits)

  #format sd data
  results_sd[,-1]=lapply(results_sd[,-1], formatC, format = "e", digits = digits)

  #name for cycle
  if(mode=="recipes"){
    if(value=="day"){
      #24 hour in 1 day
      colnames(results)[1]="hour of day"
    }else if(value=="week"){
      #7 days in 1 week
      colnames(results)[1]="day of week"
    }else if(value=="month"){
      #31 days in 1 month
      colnames(results)[1]="day of month"
    }else if(value=="year"){
      #12 month in 1 year
      colnames(results)[1]="month of year"
    }
  }else if(mode=="ncycle"){
    colnames(results)[1]="cycle"
  }else if(mode=="custom"){
    colnames(results)[1]="custom cycle"
  }

  #rename average df
  colnames(results)[2:ncol(results)] <- cona_df[2:length(cona_df)]

  #rename sd df
  colnames(results_sd) <- colnames(results)

  #output
  df_average=results
  df_sd=results_sd
  results <- list(df_average = df_average, df_sd = df_sd)
  return(results)
}
