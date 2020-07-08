#' Demo data of air quality
#'
#' 5 days air quality data (1 min) includes: NO, NO2, CO, SO2, O3.
#' The variables are as follows:
#'
#' @format A data frame with 7140 rows and 6 variables:
#' \describe{
#'   \item{Time}{Time for data}
#'   \item{NO}{Nitric oxide (NO)}
#'   \item{NO2}{Nitrogen Dioxide (NO2)}
#'   \item{CO}{Carbon monoxide (CO)}
#'   \item{SO2}{Sulfur dioxide (SO2)}
#'   \item{O3}{Ozone (O3)}
#' }
"aqi"

#' Demo data of meteorology
#'
#' 5 days meteorology data (5 mins) includes: Temperature, Humidity,
#' Wind speed, Wind direction.
#' The variables are as follows:
#'
#' @format A data frame with 1287 rows and 5 variables:
#' \describe{
#'   \item{Time}{Time for data}
#'   \item{TEM}{Temperature}
#'   \item{HUM}{Humidity}
#'   \item{WS}{Wind speed}
#'   \item{WD}{Wind direction}
#'   }
"met"

#' Demo data of volatile organic compounds (VOCs)
#'
#' 5 days VOCs data (1 hour) includes: Propylene, Acetylene, n-Butane,
#' trans-2-Butene, Cyclohexane.
#' The variables are as follows:
#'
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{Time}{Time for data}
#'   \item{Propylene}{Propylene}
#'   \item{Acetylene}{Acetylene}
#'   \item{n.Butane}{n-Butane}
#'   \item{trans.2.Butene}{trans-2-Butene}
#'   \item{Cyclohexane}{Cyclohexane}
#' }
"voc"

#' Demo data of setup for tuv
#'
#' 5 days setup data for tuv includes: nt,	lat,	lon,	o3col.
#' The variables are as follows:
#'
#' @format A data frame with 5 rows and 5 variables:
#' \describe{
#'   \item{date}{date for each day}
#'   \item{nt}{data point for each day}
#'   \item{lat}{lat for each day}
#'   \item{lon}{lon for each day}
#'   \item{o3col}{o3 column concentration for each day}
#' }
"setup_tuv"
