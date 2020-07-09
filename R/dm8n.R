#' Calculate daily maximum-8-hour ozone
#'
#' Calculates daily maximum-8-hour ozone from ozone observation data.
#'
#' This function can calculate multiple columns of ozone observation data in 1 dataframe with 1 datetime columm (such as ozone concentration in different sites).
#'
#' @param df dataframe. dataframe for ozone.
#' @param colid numeric. Column index for datatime. By default, it equals to 1.
#' @param starthour numeric. Start hour for daily calculation. By default, it equals to 0.
#' @param endhour numeric.  End hour for daily calculation. By default, it equals to 16 which means averaging ozone between 16~23.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#' @param outputmode numeric. 1 stands for brief results (only DMAX8). 2 stands for detail results (DMAX8, D8, counts for D8). By default, it equal to 1.
#' @return brief results (only DMAX8), or list for for detail results (DMAX8, D8, counts for D8).
#' @export
#' @examples
#' dm8n(aqi[,c(1,6)], colid = 1, starthour = 0, endhour = 16, na.rm = TRUE, outputmode = 2)
#' @import lubridate
#' @importFrom stats aggregate
#' @importFrom utils stack unstack


dm8n<-function(df, colid = 1, starthour = 0, endhour=16, na.rm = TRUE, outputmode = 1){
	#move datetime to first column
	  if(colid != 1){
		df[,c(1,colid)] = df[,c(colid,1)]
		colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
	  }
	
	#In case df is not a dataframe.
	df <- data.frame(df,stringsAsFactors = FALSE)
  
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
			#D8
			st=i
			en=i+7
			D8_sam=colMeans(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1], na.rm = na.rm)
			D8_sam=stack(D8_sam)
			D8_sam=unstack(D8_sam)
			D8_sam=data.frame(t(D8_sam))
			D8_sam=data.frame(date=datelist[j],start_hour=st,end_hour=en,D8_sam)
			D8=rbind(D8,D8_sam)
			#count
			count_col=colSums(!is.na(df_tar[hour(df_tar[,1])>=st&hour(df_tar[,1])<=en,-1]), na.rm = na.rm)
			count_col=data.frame(t(count_col))
			colnames(count_col)=colnames(df_tar)[-1]
			D8_count_sam=data.frame(date=datelist[j],start_hour=st,end_hour=en,count_col)
			D8_count=rbind(D8_count,D8_count_sam)
		}
	}
	#remove first row
	D8=D8[-1,]

	#remove start & end hour columns
	D8_sub=D8[,c(1,4)]
	DMAX8=data.frame(aggregate(D8_sub[,2], by = list(D8_sub[,1]), max, na.rm=na.rm))
	colnames(DMAX8)[1]="date"
	#calculate DMAX8
	for (p in 5:ncol(D8)){
		D8_sub=D8[,c(1,p)]
		DMAX8_sub=data.frame(aggregate(D8_sub[,2], by = list(D8_sub[,1]), max, na.rm=na.rm))
		DMAX8=cbind(DMAX8,DMAX8_sub[2])
	}
	#set colnames
	colnames(DMAX8)[2:ncol(DMAX8)]=colnames(D8)[4:ncol(D8)]

	#remove duplicated column.
	if(ncol_ori==2){
		D8 = D8[,-ncol(D8)]
		D8_count = D8_count[,-ncol(D8_count)]
		DMAX8 = DMAX8[,-ncol(DMAX8)]
	}

	#set output
	if(outputmode==2){
		results = list(D8=D8, D8_count=D8_count, DMAX8= DMAX8)
	}else{
		results = DMAX8
	}
}
