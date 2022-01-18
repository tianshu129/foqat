#' Convert time series into proportion time series
#'
#' Convert time series into proportion time series.
#'
#' @param df dataframe of time series.
#' @param cmcase logical value. Set to TRUE if you only 
#' want to retain cases which are complete, i.e., have 
#' no missing values. The default vaule is FALSE.
#' @return  a dataframe with proportion time series.
#'
#' @export
#' @examples
#' prop(voc)
#' @importFrom stats complete.cases

prop<-function(df, cmcase=FALSE){
	if(cmcase==TRUE){df=df[complete.cases(df), ]}
	df[,-1]=df[,-1]/rowSums(df[,-1], na.rm=TRUE) *NA^!rowSums(!is.na(df[,-1]))
	return(df)
}