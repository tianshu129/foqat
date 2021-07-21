#' Calculate average variation of particle number concentration
#'
#' Calculates average variation of particle number concentration. (contain but not limited to:
#' average daily variation, average monthly variation, average annual variation)
#'
#' @param df dataframe of time series. The columns order should be: datetime, midrange of channel, dN_dlogdp.
#' @param bkip the basic time reslution for average variation, such as '1 hour'.
#' @param mode for calculating cycles: "recipes", "ncycle", "custom".
#' "recipes" means using internal setting for calculation.
#' "ncycle" means setting number of items for per cycle.
#' "custom" means using 1 column in dataframe as a list of grouping elements for calculation.
#' @param value for detail setting of mode. Possible values for "recipes" are "day", "week", "month", year".
#' "day" equals to 24 (hours) values in 1 day.
#' "week" equals to 7 (days) values in 1 week.
#' "month" equals to 31 (days) values in 1 month.
#' "year" equals to 12 (months) values in 1 year.
#' values for "ncycle" is a number representing number of items in per cycle.
#' values for "custom" is a number representing column index in dataframe.
#' @param st start time of resampling. The default value is the fisrt value of datetime column.
#' @param et end time of resampling. The default value is the last value of datetime column.
#' @param na.rm logical value. Remove NA value or not?
#' @param digits numeric value, digits for result dataframe.
#' @param ybk numeric vector, log breaks of y axis of plot.
#' @param nlmt numeric value, uplimit of dNdlogdp colorscales of plot.
#' @param colsz numeric value, size of columns in plot.
#' @return a list with 2 dataframe (average and SD) and 1 plot. The first column of dataframe is the serial number within the period. The second column is for the midranges of channels. The Third column is for the particle number concentration. \cr
#' Note that when the pattern USES
#' "ncycle" or "custom", the start time determines the start time of the first
#' element in the average variation. For example, if the first timestamp of data is
#' "2010-05-01 12:00:00", the resolution is 1 hour, the mode is "ncycle", and the
#' value is 24, then the result represents diurnal variation starting from 12 o'clock.

#' @export
#' @importFrom dplyr full_join
#' @importFrom stats aggregate
#' @importFrom lubridate duration
#' @importFrom ggplot2 ggplot geom_point scale_y_log10 scale_fill_gradientn guide_colorbar annotation_logticks labs
#' @importFrom graphics plot
#' @importFrom grDevices colorRampPalette

avrip<-function(df, bkip, mode = "recipes", value = "day", st = NULL, et = NULL, na.rm = TRUE, digits = 2, ybk=c(10,100,500,1000),nlmt=20000,colsz=10){

  #time resampling
  rs_df <- trsp(df, bkip, st = st, et = et, na.rm = na.rm)

  #set colnames of rs_df
  cona_df <- colnames(rs_df)
  #colnames(rs_df)=c("Datetime", "Midrange", "dN_dlogdp")

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
  eval(parse(text = paste(c("results <- aggregate(rs_df[,3], list(cycle=mod_list,", colnames(df)[2], "=rs_df[,2]), mean, na.rm = na.rm)"),collapse = "")))

  #sd
  eval(parse(text = paste(c("results_sd <- aggregate(rs_df[,3], list(cycle=mod_list,", colnames(df)[2], "=rs_df[,2]), sd, na.rm = na.rm)"),collapse = "")))


  #format average data (avoid NA)
  if(!all(is.na(results[, -1]))){
	if(ncol(results)==2){
		results[,-1]=do.call(rbind, lapply(results[,-1], formatC, format = "e", digits = digits))
	}else{
		results[,-1]=lapply(results[,-1], formatC, format = "e", digits = digits)
	}
  }

  #format sd data (avoid NA)
  if(!all(is.na(results_sd[, -1]))){
	if(ncol(results_sd)==2){
		results_sd[,-1]=do.call(rbind, lapply(results_sd[,-1], formatC, format = "e", digits = digits))
	}else{
		results_sd[,-1]=lapply(results_sd[,-1], formatC, format = "e", digits = digits)
	}
  }

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
  
  #plot
  colors <- colorRampPalette(c("purple","royalblue","seagreen","orange","red"))(500)
   
   p=eval(parse(text = paste(c("ggplot() + geom_point(data=results, aes(x=", colnames(results)[1], ", y=", colnames(results)[2], ", fill=ifelse(", colnames(results)[3], "<nlmt,", colnames(results)[3], ",ifelse(", colnames(results)[3], ">=nlmt,nlmt,", colnames(results)[3], "))),shape=22, color='transparent',size=colsz) + scale_y_log10(breaks = ybk,labels = ybk) + scale_fill_gradientn(limits = c(0,nlmt),colors= colors,name='dn/dlogdp',guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),na.value='transparent') + annotation_logticks(sides = 'lr') + labs(x = 'Datetime', y = 'Midrange')"),collapse = "")))
	plot(p)
  
  #output
  df_average=results
  df_average[,-1] <- sapply(df_average[,-1],as.numeric)
  df_sd=results_sd
  df_sd[,-1] <- sapply(df_sd[,-1],as.numeric)
  results <- list(df_average = df_average, df_sd = df_sd,img=p)
  return(results)
}
