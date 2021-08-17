#' Convert the format of particle size data
#'
#' Converting the format of particle size data. There are 2 types of particle size data: table and list.
#' For table format: the first column of input is datetime; the other column is the number concentration of each particle size channel, column name is the middle particle size of the particle size channel.  
#' For list format: the first column of input is datetime. The second column of input is for middle ranges of channels. 
#' The third column of input is for particle number concentration of each channel at each timepoint.
#'
#'
#' @param df dataframe of particle size data: a table or a list. 
#' @return a dataframe. If the input is a table, the output is a list, and if the input is a list, the output is a table. 
#' @export
#' @importFrom reshape2 dcast melt

transp <- function(df){
	#remove rows showing NA in datatime
	df=df[!is.na(df[,1]),]
	
	temp_name=names(df)[1]
	names(df)[1]="Datetime"
	if(ncol(df)==3){
		#remove duplicated row by time and midrange
		ind <- duplicated(df[,1:2])
		df=df[!ind,]
		listp=df		
		tablep=eval(parse(text = paste(c("dcast(df,", names(df)[1], " ~ ", names(df)[2], ")"),collapse = "")))
	}else{
		listp=eval(parse(text = paste0(c("melt(df, id.vars = '", names(df)[1] ,"')"),collapse = "")))
		listp[,2]=as.numeric(unlist(regmatches(listp[,2],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",listp[,2]))))
		listp[,2]=as.numeric(listp[,2])
		tablep=df
	}		
  
	#output
	if(ncol(df)==3){
		names(tablep)[1]=temp_name
		results <- tablep
	}else{
		names(listp)[1]=temp_name
		results <- listp
	}
	return(results)	
}
