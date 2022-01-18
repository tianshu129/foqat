#' Calculate TUV in batch
#'
#' This function runs TUV in batch by reading the time series for the
#' parameters to be entered, and summarizes the results to the new dataframe. \cr
#'
#' There are online and offline versions of the TUV model, but both need to run on
#' a daily basis (that means manually reset parameters for each day's simulation). \cr
#' This function runs TUV in batch by reading the time series for the
#' parameters to be entered, and summarizes the results to the new dataframe. \cr
#' Currently only mode 2 (mode that outputs the photolysis rates) is supported. \cr
#' Logical variables are not supported currently!!! \cr
#' Please download \href{https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip}{TUV executable for Windows} before you use this function.
#'
#' Columns of photolysis rate coefficients (s-1): \cr
#' 1 = O3 -> O2 + O(1D) \cr
#' 2 = H2O2 -> 2 OH \cr
#' 3 = NO2 -> NO + O(3P) \cr
#' 4 = NO3 -> NO + O2 \cr
#' 5 = NO3 -> NO2 + O(3P) \cr
#' 6 = CH2O -> H + HCO \cr
#' 7 = CH2O -> H2 + CO \cr
#'
#' @param pathtuv path for parent folder of TUV executable for Windows, such as "c:/tuv5.3.1.exe".
#' @param df dataframe of the time series for the parameters to be entered, such as 'date', 'o3col'. It must includes date column.
#' @param colid column index of date. The default value is 1.
#' @return a dataframe.The first column is datetime. The second column is the solar altitude Angle. The rates of photolysis for each reaction(Unit: s-1)
#' start from third column: 1 = O3 -> O2 + O1D
#'
#' @export
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate hours
#' @importFrom lubridate minutes

tuv <- function(pathtuv, df, colid = 1){
outfile="usrout.txt"
#Store the original work path
oldwd=getwd()
on.exit(setwd(oldwd))
#Set the TUV directory to the current working path
setwd(paste(c(pathtuv,"/tuv"),collapse =""))
#In case df is not a dataframe.
df<- data.frame(df,stringsAsFactors = FALSE)
#set date to first column
if(colid != 1){
  df[,c(1,colid)] = df[,c(colid,1)]
  colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
}
# read a row at a time
for(irow in 1:nrow(df)){
	#Write date
	iyear=format(df[irow,1],'%Y')
	imonth=format(df[irow,1],'%m')
	iday=format(df[irow,1],'%d')
	cmdtxt=paste(c("iyear\n", iyear, "\nimonth\n", imonth, "\niday\n", iday, "\n"),collapse ="")
	# write other parameters
	for(icol in 2:ncol(df)){
		#combine part of the cmd
		cmdtxt=paste(c(cmdtxt, colnames(df)[icol], "\n", df[irow,icol], "\n"),collapse ="")
	}
	#Finally, add the selection mode at the beginning and the three return commands at the end
	cmdtxt=paste(c("\n2\n",cmdtxt,"\n\n\n"),collapse ="")
	system(command="tuv", input=cmdtxt)
	#Read usrout.txt
	result <- data.frame(readLines(paste(c(pathtuv,"/", outfile),collapse ="")), stringsAsFactors = FALSE)
	result_j <- data.frame(result[(which(grepl("deg.", result[,1]))[2]+1):(nrow(result)-1),])
	result_j_df <- data.frame(str_split_fixed(result_j[,1], "\\s+", 10), stringsAsFactors = FALSE)[,2:10]
	colnames(result_j_df) <- c("time hrs.", "sza deg.", "1", "2", "3", "4", "5", "6", "7")
	hours_result = round(as.numeric(result_j_df[,1]))
	minutes_result = round((as.numeric(result_j_df[,1])-round(as.numeric(result_j_df[,1])))*60)
	result_j_df[,1] <- as.character(as.Date(df[irow,1]) + hours(hours_result) + minutes(minutes_result))
	# Writes result data to the result df
	if(exists("result_j_df_all")){
		result_j_df_all <- rbind(result_j_df_all, result_j_df)
	}else{
		result_j_df_all <- result_j_df
	}
}
# Restore the working path
setwd(oldwd)
result_j <- result_j_df_all
}
