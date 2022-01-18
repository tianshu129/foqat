#' Calculate daily maximum-8-hour ozone in batch
#'
#' Calculates daily maximum-8-hour ozone in batch
#'
#' This function can calculate daily maximum-8-hour ozone in batch.
#' @param df dataframe of time series for ozone and other related parameters.
#' @param starthour numeric, start hour for calculating 8-hour ozone. By default, it equals to 0.
#' @param endhour numeric, end hour for calculating 8-hour ozone. By default, it equals to 16 which means averaging ozone between 16~23.
#' @param nh numeric. The number of effective hourly concentrations per 8-hour period.
#' @param nc numeric. The number of effective 8-hour average concentrations per day.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#' @param outputmode numeric, the format of the output, possible value: 1 or 2. See 'value' for the results of filling in 1 or 2.
#' @return a dataframe depends on the value of
#' 'outputMode'. Value 1 will output 1 list which incudes
#' 1 table (maximum-8-hour ozone). Value 2 will output 
#' 1 list which contains 4 tables (8-hour ozone, 
#' statistics of the number of effective hourly 
#' concentrations in each 8-hour average concentration, 
#' statistics of the number of effective 8-hour average
#'  concentrations in each day, maximum-8-hour ozone).
#'
#' @export
#' @importFrom dplyr left_join

dm8n_batch<-function(df, starthour = 0, endhour=16, nh=6, nc=14, na.rm = TRUE, outputmode = 1){	
	xi_df=df[,c(1,2)]
	xi=dm8n_np(xi_df, colid = 1, colio = 2, starthour = 0, endhour=16, nh=6, nc=14, na.rm = TRUE, outputmode = 2)
	xi_D8_final=xi[["D8"]]
	xi_D8_count_final=xi[["D8_count"]]
	xi_D8_count_by_day_final=xi[["D8_count_by_day"]]
	xi_DMAX8_final=xi[["DMAX8"]]
	
	if(ncol(df)>2){
		for(i in 3:ncol(df)){
			xi_df=df[,c(1,i)]
			xi=dm8n_np(xi_df, colid = 1, colio = 2, starthour = 0, endhour=16, nh=6, nc=14, na.rm = TRUE, outputmode = 2)
			xi_D8=xi[["D8"]]
			xi_D8_count=xi[["D8_count"]]
			xi_D8_count_by_day=xi[["D8_count_by_day"]]
			xi_DMAX8=xi[["DMAX8"]]
			#left_join
			xi_D8_final= left_join(xi_D8_final, xi_D8, by = names(xi_D8_final)[c(1,2,3)])
			xi_D8_count_final= left_join(xi_D8_count_final, xi_D8_count, by = names(xi_D8_count_final)[c(1,2,3)])
			xi_D8_count_by_day_final= left_join(xi_D8_count_by_day_final, xi_D8_count_by_day, by = names(xi_D8_count_by_day_final)[1])
			xi_DMAX8_final= left_join(xi_DMAX8_final, xi_DMAX8, by = names(xi_DMAX8_final)[1])
		}
	}

	#set out  put
	if(outputmode==2){
	  names(xi_D8_count_final)[c(-1,-2,-3)]=names(df)[-1]
	  names(xi_D8_count_by_day_final)[-1]=names(df)[-1]
	  results = list(D8=xi_D8_final, D8_count=xi_D8_count_final, D8_count_by_day=xi_D8_count_by_day_final, DMAX8=xi_DMAX8_final)
	}else{
	  results = xi_DMAX8_final
	}

} 