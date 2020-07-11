# FOQAT

## Tableof Contents
* [GENERAL OVERVIEW](#general-overview)
* [INSTALLATION OF R & RSTUDIO](#Rinstallation-of-r--rstudio)
* [BREIF KNOWLEDGE OF R](#breif-knowledge-of-r)
* [INSTALLATION OF FOQAT](#installation-of-foqat)
* [FUNCTIONS AND EXAMPLES](#functions-and-examples)
* [GETTING HELP](#getting-help)
* [DONATION](#DONATION)

## GENERAL OVERVIEW

The FOQAT package is a toolkit for rapid processing and analysis of atmospheric field observations and air pollution data based on R.

Features currently included:

* [Summary data (statdf)](#summary-data-statdf)

* [Resample time series (trs) (Return complete time series. Wind data is acceptable.)](#resample-time-series-trs)

* [Calculate average of variation (avri) (Wind data is acceptable.)](#calculate-average-of-variation-avri)

* [Calcualte Ozone Formation Potential (ofp)](#calcualte-ozone-formation-potential-ofp)

* [Calculate daily maximum-8-hour ozone (dm8n)](#calculate-daily-maximum-8-hour-ozone-dm8n)

* [Get OH Reactivity (koh)](#oh-reactivity-koh)

* [Calculate TUV in batch (tuv)](#calculate-tuv-in-batch-tuv)

The package contains 4 random sample data sets (5 days in length) to demonstrate functions:

* Air quality data set (aqi)

* Meteorological data set (met)

* Volatile organic compounds data set (voc)

* Demonstration dataset to configure the TUV (setup_tuv)


## INSTALLATION OF R & RSTUDIO
Please install the R program first, then install the R integrated development platform Rstudio (Desktop version).Then open Rstudio and enter the code from the console at the bottom right of the interface.Please click on the link:  
[R](https://cloud.r-project.org/)    
[rstudio-desktop](https://rstudio.com/products/rstudio/#rstudio-desktop)    

## BREIF KNOWLEDGE OF R

### FUNCTIONS IN R
----------
In R language, operation is conducted by function. 

Functions exist in the R package.

To run the function, you need to call the installed package first. For example, the following statement calls the readxl package, which contains functions for reading excel files:  
``` r
library(readxl)
```

The default R comes with only a few base functions.  Installation of Additional packages are required to obtain the additional functions. R packages are divided into CRAN and non-CRAN packages CRAN packages are certificated by R and can be download from R  server.The installation methods are divided into online and offline. The following describes the online installation method:
installation method for CRAN package:
``` r
install.packages("readxl")
```
installation method for non-CRAN package:  
``` r
#Install the CRAN package ‘remotes’
install.packages("remotes")
#Call the 'install_github' function to install non-CRAN package
remotes::install_github("tianshu129/foqat")
```

Function structure is "function name(parameter 1, parameter 2, etc.)". The different parameters are separated with commas. The result of running function can be assigned to any variable. For example, the following code will generate a vector:
``` r
x=rep(1,3)
View(x)
```

In addition, it is important to note that some arguments have default values. Defaults are listed in the function usage. For example, "Function A (parameter 1, parameter 2 = default value of parameter 2, ··)", then in function A, parameter 1 has no default value and parameter 2 has default value.  
If the default value meets your requirements, you don't need to list this parameter. For example, the following 2 line code get the same result (assuming you want to set n to 2) :
``` r
statdf(x, n = 2)
statdf(x)
```

### READ AND WRITE DATA
----------
There are many ways to read and write data in R. Here are the most common ones.
You can put the data in an XLSX file and read it into any variable in R through the read_xlsx function in the 'readxl' package. Such as:
``` r
library(readxl)
#Many default parameters are skipped here.The first parameter is the file path, the second parameter is the data type of each column, the third parameter reads NA value as null, and the fourth parameter is the SHEET number. Read the data and assign the value to the variable df.
df <- read_xlsx("E:/Users/Chen/Desktop/input.xlsx", col_types = c("date",rep("numeric",7)), na = "", sheet = 1)
```

The built-in function write.csv can be used to write the data into the CSV file, for example:
``` r
#Many default parameters are skipped here.The first parameter is the data variable to output, the second parameter is the output path, and the third parameter is set to not output row number.
write.csv(result,"E:/Users/Chen/Desktop/tuv_result.csv",row.names=F)
```

## INSTALLATION OF FOQAT
This package is not currently collected by CRAN, you need to install the CARN package 'Remotes'.Then install this package using the 'install_github' function in "remotes".
``` r
#Install the CRAN package ‘remotes’
install.packages("remotes")
#Call the 'install_github' function to install this package
remotes::install_github("tianshu129/foqat")
```

## FUNCTIONS AND EXAMPLES


### Summary data (statdf) 
----------
* #### Description

Summary of data, including：Mean, variance, min, Max, fractional values (25%, 50%, 75%).

* #### Usage
``` r
statdf(x, n = 2)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `x`               | dataframe of time series          |                            |                                      |
| `n`               | digits for reuslt in dataframe.   | 2                          |                                      |

* #### Output

Output a dataframe, columns stands for parameters, rows stands for variables.

* #### Examples

``` r
x = statdf(aqi, n = 2)
View(x)
```


### Resample time series (trs)
----------
* #### Description

Resample time series, and returns complete time series with new time resolution. (wind data is acceptable)

* #### Usage
``` r
trs(df, bkip, colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                     |
| `bkip`            | the new time reslution for resampling|                         | '1 hour'                             |
| `colid`           | column index for date             |                            |                                      |
| `st`              | start time of resampling          | NULL                       | The default value is the fisrt value of datetime column. |
| `et`              | end time of resampling            | NULL                       | The default value is the last value of datetime column. |              
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `wind`            | logical value. if TRUE, please set coliwd, coliws. | FALSE     |                                      |
| `coliws`          | numeric value, colindex of wind speed in dataframe. | 2        |                                      |
| `coliwd`          | numeric value, colindex of wind direction (degree) in dataframe. | 3   Unit for wind is dgree.        |

* #### Output

Output is a dataframe which contain a time series with a new time resolution.

* #### Examples

``` r
x = trs(met, bkip = "1 hour", colid = 1, st = "2017-05-01 00:00:00", et = NULL, na.rm = TRUE, wind = TRUE, coliws = 4, coliwd = 5)
View(x)
```


### Calculate average of variation (avri)
----------
* #### Description

Calculates average of variation of time series. (contain but not limited to: average daily variation, average monthly variation, average annual variation)

* #### Usage
``` r
avri(df, bkip, mode = "recipes", value = "day", colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `bkip`            | the basic time reslution for average variation|                            | If you want to calculate the diurnal varivation in hourly resolution, write '1 hour'.|
| `mode`            | mode for calculating cycles       |"recipes"                   | 3 modes: "recipes", "ncycle", "custom". "recipes" means using internal setting for calculation. "ncycle" means setting number of items for per cycle. "custom" means using 1 column in dataframe as a list of grouping elements for calculation.|
| `value`           | value for deail seting of mode    |"day"                       | Possible values for "recipes" are "day", "week", "month", year". "day" equals to 24 (hours) values in 1 day. "week" equals to 7 (days) values in 1 week. "month" equals to 31 (days) values in 1 month. "year" equals to 12 (months) values in 1 year. values for "ncycle" is a number representing number of items in per cycle. values for "custom" is a number representing column index in dataframe.|
| `colid`           | column index for date             |                            |                                      |
| `st`              | start time of resampling          | NULL                       | The default value is the fisrt value of datetime column. |
| `et`              | end time of resampling            | NULL                       | The default value is the last value of datetime column. |              
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `wind`            | logical value. if TRUE, please set coliwd, coliws. | FALSE     |                                      |
| `coliws`          | numeric value, colindex of wind speed in dataframe. | 2        |                                      |
| `coliwd`          | numeric value, colindex of wind direction (degree) in dataframe. | 3   Unit for wind is dgree.        |

* #### Output

The output is a dataframe. The first column is the serial number within the period. The average variation start from the second column. Note that when the pattern USES "ncycle" or "custom", the start time determines the start time of the first element in the average variation. For example, if the first timestamp of data is "2010-05-01 12:00:00", the resolution is 1 hour, the mode is "ncycle", and the value is 24, then the result represents diurnal variation starting from 12 o'clock.

* #### Examples

``` r
x = avri(met, bkip = "1 hour", mode = "recipes", value = "day", colid = 1, st = "2017-05-01 00:00:00", et = NULL, na.rm = TRUE, wind = TRUE, coliws = 4, coliwd = 5)
View(x)
View(x[["df_average"]])
View(x[["df_sd"]])
```


### Calcualte Ozone Formation Potential (ofp)
----------
* #### Description
Calculate Ozone Formation Potential (OFP) of VOC time series.  
The CAS number was matched for each VOC speices (from column name), and the Maximum Incremental Reactivity (MIR) value was matched through the CAS number and used for time series calculation.  
The MIR value comes from “Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339” (revised January 28, 2010).  
* #### Usage
``` r
ofp(df, unit = "ugm", t = 25, p = 101.325, colid = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `unit`            | unit for VOC data (micrograms per cubic meter or PPB).Please fill in "UGM" or "PPB" in quotation marks. | "ugm"                     |  |
| `t`               | Temperature, in units k, for conversion from PPB to micrograms per cubic meter.| 25|                  |
| `p`               | Pressure, in kPa, for converting from PPB to micrograms per cubic meter.| 101.325|                    |
| `colid`           | column index for date             |                            |                                      |

* #### Output

Output is a list containing 2 tables: results for matched MIR values and OFP time series.

* #### Examples

``` r
x = ofp(voc, unit = "ppb", t = 25, p = 101.325, colid = 1)
View(x)
View(x[["MIR_Result"]])
View(x[["OFP_Result"]])
```


### Calculate daily maximum-8-hour ozone (dm8n)
----------
* #### Description

Calculates daily maximum-8-hour ozone from ozone observation data.

* #### Usage
``` r
dm8n(df, colid = 1, starthour = 0, endhour = 16, na.rm = TRUE, outputmode = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `colid`           | column index for date             |                            |                                      |
| `starthour`       | start hour for calculating 8-hour ozone | 0                    |                                      |
| `endhour`         | end hour for calculating 8-hour ozone   | 16     |16 is the average of 8-hour ozone between 16 and 23.|
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `outputmode`      | the format of the output, possible value: 1 or 2.| 1 |See output for the results of filling in 1 or 2.|

* #### Output

Depends on the value of 'outputMode'. Value 1 will output 1 table: maximum-8-hour ozone. Value 1 will output 1 list, which contains 3 tables: 8-hour ozone, statistics of number of valid items within each calculation interval, and maximum-8-hour ozone.

* #### Examples

``` r 
x = dm8n(aqi[,c(1,6)], colid = 1, starthour = 0, endhour = 16, na.rm = TRUE, outputmode = 2)
View(x)
View(x[["D8"]])
View(x[["D8_count"]])
View(x[["DMAX8"]])
```


### Get OH Reactivity (koh)
----------
* #### Description

Theoretical values of the SPECIES 'OH reaction constant kOH at 25 degrees were obtained from' Chemspider.com '. Value source: US Environmental Protection Agency’s EPISuite。

* #### Usage
``` r
koh(spec)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `spec`            |  species                          |          |The name of the species or CAS number are both acceptable.|

* #### Output

Output is the theoretical value of the species' OH reaction constant kOH at 25 degrees.

* #### Examples

``` r
koh("propane")
```


### Calculate TUV in batch (tuv)
----------
* #### Description

There are online and offline versions of the TUV model, but both need to run on a daily basis (that means manually reset parameters for each day's simulation). This function runs TUV in batch by reading the time series of values for the parameters to be entered, and summarizes the results to the new dataframe.
Currently only mode 2 (mode that outputs the photolysis rate) is supported.
Before you use this function, please download [TUV executable for Windows](https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip)。
* #### Usage
``` r
tuv(pathtuv, df, colid = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `pathtuv`         | path for parent folder of TUV Windows program |                |"G:/tuv5.3.1.exe"                     |
| `df`              | the time series of values for the parameters to be entered|   |The date column must be included.      |
| `colid`           | column index for date             |                            |                                      |

* #### Output

Output a dataframe.The first column is datetime. The second column is the solar altitude Angle. The rates of photolysis for each reaction(Unit: s-1) start from third column:
1 = O<sub>3</sub> -> O<sub>2</sub> + O<sub>1D</sub>  
2 = H<sub>2</sub>O<sub>2</sub> -> 2 OH  
3 = NO<sub>2</sub> -> NO + O<sub>3P</sub>  
4 = NO<sub>3</sub> -> NO + O<sub>2</sub>   
5 = NO<sub>3</sub> -> NO<sub>2</sub> + O<sub>3P</sub>  
6 = CH<sub>2</sub>O -> H + HCO  
7 = CH<sub>2</sub>O -> H<sub>2</sub> + CO  

* #### Examples

``` r
#please use your path for master folder of tuv in "pathtuv".
x=tuv(pathtuv, setup_tuv, colid = 1)
View(x)
```

## GETTING HELP

Please send email to: tianshu129@163.com  

## DONATION

The project took a lot of time and effort.If this project is helpful to you, you are welcome to donate. Thank you very much:
<img src="./img/donation.jpg" width="200" height="200" alt="支付" align=center>
