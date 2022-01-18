#' Summary of dataframe
#'
#' Summary of dataframe.
#'
#' Summary of dataframe: mean, standard deviation (sd), minimum (min), percentiles
#' (0.25, 0.50, 0.75), maximum (max).
#'
#' @param df dataframe of time series.
#' @param n digits for reuslt in dataframe.
#' @param cmcase logical value. Set to TRUE if you only 
#' want to summary cases which are complete, i.e., have 
#' no missing values.
#' @param prop logical value. Convert time series into 
#' proportion time series before summary.
#' @return  a dataframe, columns stands for parameters, 
#' rows stands for variables.
#' @export
#' @examples
#' statdf(voc)
#' @importFrom stats quantile sd complete.cases

statdf = function(df, n = 2, cmcase=FALSE, prop=FALSE){
  if(cmcase==TRUE){df=df[complete.cases(df), ]}
  if(prop==TRUE){df=prop(df)}
  x=df[,-1]
  mean_df <- function(x) {if (is.numeric(x)) round(mean(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  sd_df <- function(x) {if (is.numeric(x)) round(sd(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  min_df <- function(x) {if (is.numeric(x)) round(min(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  q25_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.25, na.rm=TRUE), digits=n) else "Non numeric type"}
  q50_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.5, na.rm=TRUE), digits=n) else "Non numeric type"}
  q75_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.75, na.rm=TRUE), digits=n) else "Non numeric type"}
  max_df <- function(x) {if (is.numeric(x)) round(max(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  sum_df <- cbind(sapply(x, mean_df), sapply(x, sd_df), sapply(x, min_df), sapply(x, q25_df), sapply(x, q50_df), sapply(x, q75_df), sapply(x, max_df), round(as.matrix(colSums(!is.na(x))/nrow(x)), digits=3))
  sum_df <- as.data.frame(sum_df); names(sum_df) <- c('mean', 'sd', 'min', '25%', '50%', '75%',  'max', 'integrity')
  return(sum_df)
}
