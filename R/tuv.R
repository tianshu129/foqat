#' Calculate TUV in batch
#'
#' run offline batch calculation of \href{https://www2.acom.ucar.edu/modeling/tropospheric-ultraviolet-and-visible-tuv-radiation-model}{TROPOSPHERIC ULTRAVIOLET AND VISIBLE (TUV) RADIATION MODEL}. \cr
#' Currently, this function only support output of photolysis rate coefficients (J-values). \cr
#' Columns of photolysis rate coefficients (s-1): \cr
#' 1 = O3 -> O2 + O(1D) \cr
#' 2 = H2O2 -> 2 OH \cr
#' 3 = NO2 -> NO + O(3P) \cr
#' 4 = NO3 -> NO + O2 \cr
#' 5 = NO3 -> NO2 + O(3P) \cr
#' 6 = CH2O -> H + HCO \cr
#' 7 = CH2O -> H2 + CO \cr
#' Please download \href{https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip}{TUV executable for Windows} before you use this function. \cr
#'
#' @param pathtuv path for TUV folder, such as "c:/tuv5.3.1.exe".
#' @param df dataframe for variable.
#' @param colt column index of date.
#' @return result_j dataframe of photolysis rate coefficients (J-values).
#' @export
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate hour

tuv <- function(pathtuv, df, colt){
outfile="usrout.txt"
#Store the original work path
oldwd=getwd()
#Set the TUV directory to the current working path
setwd(paste(c(pathtuv,"/tuv"),collapse =""))
#In case df is not a dataframe.
df<- data.frame(df,stringsAsFactors = FALSE)
#set date to first column
if(colt!=1){df[,c(1,colt)] <- df[,c(colt,1)]}
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
	result_j_df[,1] <- as.character(hours(as.numeric(result_j_df[,1])) + as.Date(df[irow,1]))
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
