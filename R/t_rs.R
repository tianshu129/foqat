#' Resampling time series and return complete time series with new time period.
#'
#' @param df dataframe contains time series.
#' @param df_date colname of datetime in.
#' @param start_time start time of resampling.
#' @param end_time end time of resampling.
#' @param break_input interval of resampling.
#' @param na.rm logical value. remove NA value or not?
#' @return  dataframe with new resolution. 
#' @export
#' @importFrom dplyr full_join
#' @importFrom dplyr full_join
#' @importFrom stats aggregate

t_rs<-function(df,df_date,start_time=NULL,end_time=NULL,break_input, na.rm = TRUE){
  options(warn=-1)
  #aggregate、seq need space 
  if(!grepl("\\s", break_input)){
    #if not space in break_input
    break_input<-gsub("([0-9])([a-z])", "\\1 \\2", break_input)
  }
  #set colname for datetime
  colnames(df)[colnames(df)==df_date]="timestamp"
  #set format for datetime
  df$timestamp=as.POSIXct(df$timestamp,'%Y-%m-%d %H:%M',tz= "GMT")
  #input start_time, end_time?
  if(is.null(start_time)){
    #if not input start_time, set first timestamp as start_time
    start_time=as.POSIXct(df$timestamp[1],'%Y-%m-%d %H:%M',tz= "GMT")
  }else{
    #if input start_time, cut df by start_time
    start_time=as.POSIXct(start_time,format="%Y-%m-%d %H:%M",tz= "GMT")
    ##if start_time not exist in df, insert start_time
    if(start_time!=df$timestamp[min(which(df$timestamp >= start_time))]){
      k=min(which(df$timestamp >= start_time))
      df[seq(k+1,nrow(df)+1),] <- df[seq(k,nrow(df)),]
      df[k,"timestamp"] <- as.POSIXct(start_time,tz="GMT")
      df[k,!colnames(df)%in%"timestamp"] <- c(rep(NA, ncol(df)-1))
    }
    df<-df[df$timestamp>=start_time,]
  }
  if(is.null(end_time)){
    #if not input end_time, set last timestamp as end_time
    end_time<-as.POSIXct(as.character(df$timestamp[length(df$timestamp)]),'%Y-%m-%d %H:%M')
  }else{
    #if input end_time, cut df by end_time
    end_time=as.POSIXct(end_time,format="%Y-%m-%d %H:%M",tz= "GMT")
    ##if end_time not exist in df, insert end_time
    if(end_time!=df$timestamp[max(which(df$timestamp <= end_time))]){
      k=max(which(df$timestamp <= end_time))
      df[seq(k+1,nrow(df)+1),] <- df[seq(k,nrow(df)),]
      df[k,"timestamp"] <- as.POSIXct(end_time,tz="GMT")
      df[k,!colnames(df)%in%"timestamp"] <- c(rep(NA, ncol(df)-1))
    }
    df<-df[df$timestamp<=end_time,]
  }
  datat<-aggregate(df[,-1], list(timestamp = cut(df$timestamp,breaks=	break_input)),mean,, na.rm = na.rm)
  datat$timestamp=as.POSIXct(datat$timestamp,"%Y-%m-%d %H:%M",tz="GMT")
  #gnerate timeseries
  start_time<-as.POSIXct(datat$timestamp[1],'%Y-%m-%d %H:%M',tz="GMT")
  end_time<-as.POSIXct(datat$timestamp[length(datat$timestamp)],'%Y-%m-%d %H:%M',tz="GMT")
  ts <- seq.POSIXt(start_time,end_time, by=	break_input)
  df <- data.frame(timestamp=ts)
  #complete timestamp
  df <- full_join(df,datat)
  options(warn=0)
  return(df)
}
