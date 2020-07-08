#' Summary of dataframe
#'
#' Summary of dataframe.
#'
#' Summary of dataframe: mean, sd, min, pecentag evalue (25%, 50%, 75%), max.
#'
#' @param x dataframe contains time series.
#' @param n digits for reuslt in dataframe.
#' @return  dataframe with new resolution.
#' @export
#' @examples
#' statdf(aqi)
#' @importFrom stats quantile sd

statdf = function(x, n = 2) {
  mean_df <- function(x) {if (is.numeric(x)) round(mean(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  sd_df <- function(x) {if (is.numeric(x)) round(sd(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  min_df <- function(x) {if (is.numeric(x)) round(min(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  q25_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.25, na.rm=TRUE), digits=n) else "Non numeric type"}
  q50_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.5, na.rm=TRUE), digits=n) else "Non numeric type"}
  q75_df <- function(x) {if (is.numeric(x)) round(quantile(x, probs=.75, na.rm=TRUE), digits=n) else "Non numeric type"}
  max_df <- function(x) {if (is.numeric(x)) round(max(x, na.rm=TRUE), digits=n) else "Not numeric type"}
  sumtable <- cbind(sapply(x, mean_df), sapply(x, sd_df), sapply(x, max_df), sapply(x, min_df), sapply(x, q25_df), sapply(x, q50_df), sapply(x, q75_df), round(as.matrix(colSums(!is.na(x))/nrow(x)), digits=3))
  sumtable <- as.data.frame(sumtable); names(sumtable) <- c('mean', 'sd', 'min', '25%', '50%', '75%',  'max', 'integrity')
  return(sumtable)
}
