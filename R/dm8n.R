#' Calculate daily maximum-8-hour ozone
#'
#' Calculates daily maximum-8-hour ozone from ozone observation data.
#'
#' This function can calculate daily maximum-8-hour ozone and other parameters corresponding to it.
#'
#' @param df dataframe of time series for ozone and other related parameters.
#' @param colid column index for date-time. By default, it equals to 1.
#' @param colio column index for ozone. By default, it equals to 2.
#' @param starthour numeric, start hour for calculating 8-hour ozone. By default, it equals to 0.
#' @param endhour numeric, end hour for calculating 8-hour ozone. By default, it equals to 16 which means averaging ozone between 16~23.
#' @param nh numeric. The number of effective hourly concentrations per 8-hour period.
#' @param nc numeric. The number of effective 8-hour average concentrations per day.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#' @param outputmode numeric, the format of the output, possible value: 1 or 2. See 'value' for the results of filling in 1 or 2.
#' @param unitlb labels for y axis of dma8 plot. By default, it equals to NA.
#' @return a dataframe depends on the value of 'outputMode'. Value 1 will output 1 list which incudes
#' 1 table (maximum-8-hour ozone) and 1 plot (dma8 plot). Value 2 will output 1 list which contains 4
#' tables (8-hour ozone, statistics of the number of effective hourly concentrations in each 8-hour average concentration, 
#' statistics of the number of effective 8-hour average concentrations in each day, maximum-8-hour ozone) and 1 plot (dma8 plot).
#' @export
#' @examples
#' dm8n(aqi,colio=6,unitlb=c("NO (ppbv)", "NO2 (ppbv)", "CO (ppbv)", "SO2 (ppbv)", "O3 (ppbv)"))
#' @import lubridate
#' @importFrom stats aggregate
#' @importFrom utils stack unstack
#' @importFrom plyr ddply .
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot
#' @importFrom reshape2 melt

dm8n<-function(df, colid = 1, colio = 2, starthour = 0, endhour=16, nh=6, nc=14, na.rm = TRUE, outputmode = 1, unitlb = NA){
  
  #In case df is not a dataframe.
  df_names <- colnames(df) 
  df <- data.frame(df,stringsAsFactors = FALSE)
  colnames(df) <- df_names
  
  #move datetime to first column
  if(colid != 1){
    df[,c(1,colid)] = df[,c(colid,1)]
    colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
  }
  
  #move ozone to second column
  if(colio != 2){
    df[,c(2,colio)] = df[,c(colio,2)]
    colnames(df)[c(2,colio)] = colnames(df)[c(colio,2)]
  }
  
  
  df <- trs(df, bkip="1 hour", na.rm = na.rm)
  
  #get data list
  datelist_raw<-as.Date(df[,1])
  datelist<-datelist_raw[!duplicated(datelist_raw)]
  
  #duplicated column if only 1 site exited.
  ncol_ori=ncol(df)
  if(ncol_ori==2){
    df$O32=df[,2]
  }
  
  #sstup dataframe by 0-7 in first day
  df_tar=df[as.Date(df[,1])==datelist[1],]
  st=starthour
  en=starthour+7
  
  D8=colMeans(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1],na.rm = na.rm)
  D8=stack(D8)
  D8=unstack(D8)
  D8=data.frame(t(D8))
  D8=data.frame(date=as.Date(df_tar[1,1]),start_hour=st,end_hour=en,D8)
  #setup df_tar fot count by 0-7 in first day
  count_col=colSums(!is.na(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1]))
  count_col=data.frame(t(count_col))
  colnames(count_col)=colnames(df_tar)[-1]
  D8_count=data.frame(date=as.Date(df_tar[1,1]),start_hour=st,end_hour=en,count_col)
  
  #loop for d8 d8_count
  for (j in 1:length(datelist)){
    #select day
    df_tar=df[as.Date(df[,1])==datelist[j],]
    print(datelist[j])
    for (i in seq(starthour,endhour,1)){			
      st=i
      en=i+7
      #count
      count_col=colSums(!is.na(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1]), na.rm = na.rm)
      count_col_nh=count_col
      count_col=data.frame(t(count_col))
      colnames(count_col)=colnames(df_tar)[-1]
      D8_count_sam=data.frame(date=datelist[j],start_hour=st,end_hour=en,count_col)
      D8_count=rbind(D8_count,D8_count_sam)
      #D8
      D8_sam=colMeans(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1], na.rm = na.rm)
      D8_sam=stack(D8_sam)
      D8_sam=unstack(D8_sam)
      D8_sam=data.frame(t(D8_sam))
      D8_sam=data.frame(date=datelist[j],start_hour=st,end_hour=en,D8_sam)
      D8=rbind(D8,D8_sam)			
    }
  }
  #remove first row
  D8=D8[-1,]
  D8_count=D8_count[-1,]
  
  #filter by 6/8 for 8-hour
  D8[D8_count[,4]<nh,4]=NA
  
  #remove start & end hour columns
  D8_sub=D8[,-c(2,3)]
  
  #calculate DMAX8
  D8_sub2=D8_sub[!is.na(D8_sub[,2]),]
  DMAX8=ddply(D8_sub2[-1], .(D8_sub2[,1]), function(x)x[x[,1]==max(x[,1]), ])
  colnames(DMAX8)[1]="date"
  
  #set colnames
  colnames(DMAX8)[2:ncol(DMAX8)]=colnames(D8)[4:ncol(D8)]
  
  #remove duplicated column.
  if(ncol_ori==2){
    D8 = D8[,-ncol(D8)]
    D8_count = D8_count[,-ncol(D8_count)]
    DMAX8 = DMAX8[,-ncol(DMAX8)]
  }
  
  #filter by 14/16 for 1-day
  D8_count[D8_count[,4]<nh,5]=0
  D8_count[D8_count[,4]>=nh,5]=1
  D8_count_by_day=D8_count[,c(1,5)]
  D8_count=D8_count[,-5]
  D8_count_by_day=data.frame(aggregate(D8_count_by_day[,2], by = list(as.Date(D8_count_by_day[,1])), sum, na.rm=TRUE))
  colnames(D8_count_by_day)[1]="date"
  DMAX8=left_join(D8_count_by_day,DMAX8)
  DMAX8=DMAX8[,-2]
  DMAX8[D8_count_by_day[,2]<nc,2]=NA
  
  #replace value to NA for rows which O3 is NA.
  DMAX8[is.na(DMAX8[,2]),2:ncol(DMAX8)]=NA
  
  #update columns
  names(D8_count)[c(1,4)] = c("date", "count")
  names(D8_count_by_day)[c(1,2)] = c("date", "count")
  
  #move ozone to last
  DMAX8_p=DMAX8
  if(ncol(DMAX8)!=2){
    DMAX8_p[,c(ncol(DMAX8_p),2)]=DMAX8_p[,c(2,ncol(DMAX8_p))]	
    colnames(DMAX8_p)[c(ncol(DMAX8_p),2)]=colnames(DMAX8_p)[c(2,ncol(DMAX8_p))]
  }
  
  #melt
  x <- melt(DMAX8_p, id.vars = c("date")) 
  
  #unitlb
  if(all(!is.na(unitlb))){
    if(length(unitlb)!=1){
      unitlb[c(length(unitlb),2)]=unitlb[c(2,length(unitlb))]
    }
    dm=unitlb
    attr(dm, 'names')=colnames(DMAX8_p)[-1]
  }else{
    dm=NA
  }
  
  #plot
  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "English") 
  DMAX8plot=ggplot(data = x, mapping = aes(x = date, y = value, shape = variable, colour = variable)) + geom_point() + geom_line() + facet_wrap(~variable, scales = "free_y", nrow = ncol(DMAX8_p), strip.position = "left", labeller = as_labeller(dm) ) + theme_bw() + theme(strip.background = element_blank(), strip.placement = "outside",axis.title.y = element_blank())
  Sys.setlocale("LC_TIME",old.loc) 
  
  #show plot
  plot(DMAX8plot)
  
  #set output
  if(outputmode==2){
    results = list(D8=D8, D8_count=D8_count, D8_count_by_day=D8_count_by_day, DMAX8=DMAX8, DMAX8plot=DMAX8plot)
  }else{
    results = list(DMAX8=DMAX8, DMAX8plot=DMAX8plot) 
  }
}
