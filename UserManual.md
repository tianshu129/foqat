# FOQAT <img src="https://s1.ax1x.com/2020/08/31/dLqtdf.png" align="right" width="120" />

![Language](https://img.shields.io/badge/Language-R-blue.svg) [![GPLv3 license](https://img.shields.io/badge/License-GPLv3-success.svg)](http://perso.crans.org/besson/LICENSE.html)
![Version](https://img.shields.io/badge/Version-1.5.17-important) 
 
## Tableof Contents
* [GENERAL OVERVIEW](#general-overview)
* [INSTALLATION OF R & RSTUDIO](#Rinstallation-of-r--rstudio)
* [BREIF KNOWLEDGE OF R](#breif-knowledge-of-r)
* [INSTALLATION OF FOQAT](#installation-of-foqat)
* [FUNCTIONS AND EXAMPLES](#functions-and-examples)
* [QUESTION OR FEEDBACK](#question-or-feedback)
* [DONATION](#DONATION)

## GENERAL OVERVIEW
The FOQAT is developed and maintained by Chen Tianshu from Professor Xue Likun's research group of Environmental Research Institute of Shandong University.  

Click the link to ask questions and give feedback (recommended) :  
☛ https://github.com/tianshu129/foqat/issues/new ☚ 

The FOQAT package is a toolkit for rapid processing and analysis of atmospheric field observations and air pollution data based on R.  

Features currently included:

* [Summary data (statdf)](#summary-data-statdf)

* [Resample time series (trs) (Return complete time series. Wind data is acceptable.)](#resample-time-series-trs)

* [Calculate average of variation (avri) (Wind data is acceptable.)](#calculate-average-of-variation-avri)

* [Batch linear regression analysis (anylm)](#batch-linear-regression-analysis-anylm)

* [Calculate daily maximum-8-hour ozone (dm8n)](#calculate-daily-maximum-8-hour-ozone-dm8n)

* [Convertion and analysis of VOC concentrations (vocct)](#convertion-and-analysis-of-voc-concentrations-vocct)

* [Calculate Ozone Formation Potential (ofp)](#calculate-ozone-formation-potential-ofp)

* [Get OH Reactivity (koh)](#oh-reactivity-koh)

* [Calculate OH reactivity (loh)](#calculate-oh-reactivity-loh)

* [Format the historical data from OpenAQ (openaq)](#format-the-historical-data-from-openaq-openaq)

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
#Many default parameters are skipped here.
The first parameter is the file path, the second parameter is the data type of each column, 
the third parameter reads NA value as null, and the fourth parameter is the SHEET number. 
Read the data and assign the value to the variable df.  
df <- read_xlsx("E:/Users/Chen/Desktop/input.xlsx", col_types = c("date",rep("numeric",7)), na = "", sheet = 1)
```

The built-in function write.csv can be used to write the data into the CSV file, for example:
``` r
#Many default parameters are skipped here.
The first parameter is the data variable to output, the second parameter is the output path, 
and the third parameter is set to not output row number.
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

Summary of dataframe: mean, standard deviation (sd), minimum (min), percentiles (0.25, 0.50, 0.75), maximum (max).

* #### Usage
``` r
statdf(x, n = 2)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
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
If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.

* #### Usage
``` r
trs(df, bkip, colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3, cpms = TRUE)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                     |
| `bkip`            | the new time reslution for resampling|                         | '1 hour'                             |
| `colid`           | column index for date-time        |1                           |                                      |
| `st`              | start time of resampling          | NULL                       | The default value is the fisrt value of datetime column. |
| `et`              | end time of resampling            | NULL                       | The default value is the last value of datetime column. |              
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `wind`            | logical value. if TRUE, please set coliwd, coliws. | FALSE     |                                      |
| `coliws`          | numeric value, column index of wind speed in dataframe. | 2        |                                      |
| `coliwd`          | numeric value, column index of wind direction (degree) in dataframe. | 3 | Unit for wind is degree.        |
| `cpms`          |  logical value. Compensate the insufficient amount of the millisecond bit for datetime column. | TRUE |   |

* #### Output

Output is a dataframe which contains a time series with a new time resolution.   

* #### Examples

``` r
x = trs(met, bkip = "1 hour", st = "2017-05-01 00:00:00", wind = TRUE, coliws = 4, coliwd = 5)
View(x)
```


### Calculate average of variation (avri)
----------
* #### Description

Calculates average of variation of time series. (contain but not limited to: average daily variation, average monthly variation, average annual variation)  
If you have wind data (wind speed, and wind direction in dgree), please set 'wind' as 'TRUE', and set values for 'coliwd' and 'coliws'.

* #### Usage
``` r
avri(df, bkip, mode = "recipes", value = "day", colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `bkip`            | the basic time reslution for average variation|                            | If you want to calculate the diurnal varivation in hourly resolution, write '1 hour'.|
| `mode`            | mode for calculating cycles       |"recipes"                   | 3 modes: "recipes", "ncycle", "custom". "recipes" means using internal setting for calculation. "ncycle" means setting number of items for per cycle. "custom" means using 1 column in dataframe as a list of grouping elements for calculation.|
| `value`           | value for detail setting of mode  |"day"                       | Possible values for "recipes" are "day", "week", "month", year". "day" equals to 24 (hours) values in 1 day. "week" equals to 7 (days) values in 1 week. "month" equals to 31 (days) values in 1 month. "year" equals to 12 (months) values in 1 year. values for "ncycle" is a number representing number of items in per cycle. values for "custom" is a number representing column index in dataframe.|
| `colid`           | column index for date-time        |1                           |                                      |
| `st`              | start time of resampling          | NULL                       | The default value is the fisrt value of datetime column. |
| `et`              | end time of resampling            | NULL                       | The default value is the last value of datetime column. |              
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `wind`            | logical value. if TRUE, please set coliwd, coliws. | FALSE     |                                      |
| `coliws`          | numeric value, column index of wind speed in dataframe. | 2        |                                      |
| `coliwd`          | numeric value, column index of wind direction (degree) in dataframe. | 3   Unit for wind is dgree.        |

* #### Output

The output is a dataframe. The first column is the serial number within the period. The average variation start from the second column. Note that when the pattern USES "ncycle" or "custom", the start time determines the start time of the first element in the average variation. For example, if the first timestamp of data is "2010-05-01 12:00:00", the resolution is 1 hour, the mode is "ncycle", and the value is 24, then the result represents diurnal variation starting from 12 o'clock.  

* #### Examples

``` r
x = avri(met, bkip = "1 hour", mode = "recipes", value = "day", colid = 1, st = "2017-05-01 00:00:00", wind = TRUE, coliws = 4, coliwd = 5)
View(x)
View(x[["df_average"]])
View(x[["df_sd"]])
```


### Batch linear regression analysis (anylm)
----------
* #### Description

Batch calculation and analysis of linear regression.Multiple species can be selected for each of the x and Y and fill color (z) dimensions.Support for a fourth dimensional operation called data grouping (T).

* #### Usage
``` r
anylm(df, xd=2, yd=3, zd=NULL, td=NULL, mi=1, range.y="interval", range.x="interval", nperm=99, scint=FALSE, dign=1, zfill="lightgray", ppsize=2, showinfo=TRUE, ptsize=12, pncol=NULL)
```
* #### parameter

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              |Time series data frame             |                            |                                      |
| `xd`              |species for x-axis (columns)       |2                 | Either column name or column nidex can be used.|
| `yd`              |species for y-axis (columns)       |3                 | Either column name or column nidex can be used.|
| `zd`              | Species used to fill the color (column) |NULL  | Either column name or column index can be used. If zd is set, the legend in the figure will display the values  corresponding to the 5 percentiles (0%, 25%, 50%, 75%, 100%).|
| `td`              | Columns used for data grouping     |NULL                        | Either column name or column index can be used. You can only select one column.        |
| `mi`              | Linear regression method           |1                     | The optional values are 1~4, which represent: ordinary least squares (OLS), major axis (MA), standard major axis (SMA), ranged major axis (RMA).|
| `range.y`         | Parameters of RMA regression method                  | "interval"       | Option are "interval" and "relative". The parameters are from the lmodel2 package. Refer to https://www.rdocumentation.org/packages/lmodel2/versions/1.7-3/topics/lmodel2|
| `range.x`         |Parameters of RMA regression method                  | "interval"                  | Option are "interval" and "relative". The parameters are from the lmodel2 package. Refer to https://www.rdocumentation.org/packages/lmodel2/versions/1.7-3/topics/lmodel2|
| `nperm`           | The number of permutations tested.                     | 99                          | The parameters are from the lmodel2 package. Refer to https://www.rdocumentation.org/packages/lmodel2/versions/1.7-3/topics/lmodel2|             |
| `showpage`        | Whether to automatically print the page containing all the pictures after getting the result | "TRUE" | |
| `scint`           | Whether the numbers in the figure use scientific notation           | "FALSE"                     |     |
| `dign`            | The number of decimal places in the figure               | 1                           |                |
| `zfill`           | When there is no species (column) fill color, specify the fill color.| "lightgray"              |       |
| `ppsize`          | Point size                      | 2                           |                                         |
| `showinfo`        | Whether to display linear regression information in the figure title         | TRUE            |        |
| `ptsize`          | Font size of figure title                   | 12                          |                             |
| `pncol`           | The number of columns in the graph on the summary page                 | NULL                       |   |

* #### Output

The output is a list, including: a list data_list containing all data sets used to calculate linear regression, a batch linear regression result summary table lm_df, a column lm_list containing all the detailed results of linear regression, a list plot_list containing all graphs, and a storage The summary page all_plot for all plots.
There are a few points to note: data_list is a multi-level list. The levels are: x->y->z (if any) -> t (if any). The data group can be indexed progressively by level, and the data group corresponding to the regression result of interest can be quickly found and exported.
The plot_list list contains all the graphs you see in the total page, and the serial number corresponds to the page id. The format is ggplot. It can be processed with ggplot2 package after export.
You can use the gridExtra::grid.arrange() function to display the total map page all_plot. You can use the ridExtra::arrangeGrob()+ggplot2::ggsave() function to output the total graph page all_plot.
   
``` r
#Examples of displaying and outputting the page all_plot overall view of the code 
gridExtra::grid.arrange(grobs=..., nrow=...)
g <- gridExtra::arrangeGrob(grobs=..., nrow=...)
ggplot2::ggsave(filename="example.jpg", plot=g)
```

* #### Examples

``` r
# Loading lubridate pack, adding additional data set represents a day calendar day by day so that packet. 
library(lubridate)
df=data.frame(aqi,day=day(aqi$Time))
# Batch linear regression analysis
x=anylm(df, xd=c(2,3), yd=6, zd=4, td=7,dign=3)
View(x)
```


### Calculate daily maximum-8-hour ozone (dm8n)
----------
* #### Description

Calculates daily maximum-8-hour/8-hour ozone from ozone observation data and other parameters corresponding to it.

* #### Usage
``` r
dm8n(df, colid = 1, colio = 2, starthour = 0, endhour=16, nh=6, nc=14, na.rm = TRUE, outputmode = 1, unitlb = NA)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series includes ozone|                            |It automatically calculates the mean of other species for the time period corresponding to the 8-hour average and maximum 8-hour average of ozone.                  |
| `colid`           | column index for date-time        |1                           |                                      |
| `colio`           | column index for ozone            |2                           |                                      |
| `starthour`       | start hour for calculating 8-hour ozone | 0                    |                                      |
| `endhour`         | end hour for calculating 8-hour ozone   | 16     |16 is the average of 8-hour ozone between 16 and 23.|
| `nh`              | The number of effective hourly concentrations per 8-hour period.| 6   |                               |
| `nc`              | The number of effective 8-hour average concentrations per day.  | 14  |                               |
| `na.rm`           | logical value. Remove NA value or not? | TRUE                  |                                      |
| `outputmode`      | the format of the output, possible value: 1 or 2.| 1 |See output for the results of filling in 1 or 2.|
| `unitlb`      | labels for y axis of dma8 plot.| NA |By default, it equals to NA. If set this parameter, the order of species should same as that in the dataframe.|

* #### Output

Depends on the value of 'outputMode'. Value 1 will output 1 list which incudes 1 table (maximum-8-hour ozone) and 1 plot (dma8 plot). Value 2 will output 1 list which contains 4 tables (8-hour ozone, statistics of the number of effective hourly concentrations in each 8-hour average concentration, statistics of the number of effective 8-hour average concentrations in each day, maximum-8-hour ozone) and 1 plot (dma8 plot).

* #### Examples

``` r 
x = dm8n(aqi,colio=6,unitlb=c("NO (ppbv)", "NO2 (ppbv)", "CO (ppbv)", "SO2 (ppbv)", "O3 (ppbv)"))
View(x)
```


### Convertion and analysis of VOC concentrations (vocct)
----------
* #### Description
Convert unit of VOCs between ugm and ppbv for VOC time series. Conduct statistics of VOC concentrations. 
The CAS number was matched for each VOC speices (from column name), and the Molecular Weight (MW) and Maximum Incremental Reactivity (MIR) value are matched through the CAS number and used for time series calculation.  
The MIR value comes from “Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339” (revised January 28, 2010).  
Note: If input VOC species contain M,P-xylene, it will be automatically divided into m-xylene and P-xylene evenly.  

* #### Usage
``` r
vocct(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, colid = 1, wamg=FALSE)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `unit`            | unit for VOC data (micrograms per cubic meter or ppbv).Please fill in "ugm" or "ppbv" in quotation marks.| "ppbv"                     |  |
| `t`               | Temperature, in Degrees Celsius, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 25|                  |
| `p`               |Pressure, in kPa, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 101.325|                         |
| `stcd`          | Does it output results in standard conditions?                |FALSE      |                              |
| `sortd`           | It determines whether the VOC species are sorted or not.      |TRUE       |Sequencing priority is: VOC groups, molecular mass, MIR value.Vocs include C ("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_organics ", "Other_Organic_Compounds", "Unknown").Among them, "Alkenes" does not include biogenic "BVOC".                                |
| `colid`          | column index for date-time        |1                           |                                         |
| `wamg`           | Should warnings be presented?      |FALSE                       |                                        |

* #### Output

Output is a list containing 9 tables: 
MW_Result: matched Molecular Weight (MW) value result;
Con_ugm: time series of VOC mass concentration by species;
Con_ugm_mean: the average mass concentration and proportion of VOC by species (sorted from large to small);
Con_ugm_group: time series of VOC mass concentration classified by groups;
Con_ugm_group_mean: the average value and proportion of VOC mass concentration (sorted from large to small) according to major groups;
Con_ppbv: time series of VOC volume concentration by species;
Con_ppbv_mean: the average volume concentration and proportion of VOC by species (sorted from large to small);
Con_ppbv_group: time series of VOC volume concentration according to major groups;
Con_ppbv_group_mean: VOC volume concentration average and proportion (sorted from large to small) according to major groups;

* #### Examples

``` r
x = vocct(voc, unit = "ppbv")
View(x)
```


### Calculate Ozone Formation Potential (ofp)
----------
* #### Description
Calculate Ozone Formation Potential (OFP) of VOC time series.  
The CAS number was matched for each VOC speices (from column name), and the Maximum Incremental Reactivity (MIR) value was matched through the CAS number and used for time series calculation.  
The MIR value comes from “Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339” (revised January 28, 2010).  
Note: If input VOC species contain M,P-xylene, it will be automatically divided into m-xylene and P-xylene evenly.  

* #### Usage
``` r
ofp(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, colid = 1, wamg=FALSE)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `unit`            | unit for VOC data (micrograms per cubic meter or ppbv).Please fill in "ugm" or "ppbv" in quotation marks.| "ppbv"                     |  |
| `t`               | Temperature, in Degrees Celsius, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 25|                  |
| `p`               |Pressure, in kPa, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 101.325|                         |
| `stcd`          | Does it output results in standard conditions?                |FALSE      |                              |
| `sortd`           | It determines whether the VOC species are sorted or not.      |TRUE       |Sequencing priority is: VOC groups, molecular mass, MIR value.Vocs include C ("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_organics ", "Other_Organic_Compounds", "Unknown").Among them, "Alkenes" does not include biogenic "BVOC".                                |
| `colid`          | column index for date-time        |1                           |                                         |
| `wamg`           | Should warnings be presented?      |FALSE                       |                                        |

* #### Output

Output is a list containing 5 tables: 
MIR_Result: matched MIR value result;
OFP_Result: OFP time series of VOC by species;
OFP_Result_mean: the average value and proportion of OFP of VOC by species (sorted from large to small);
OFP_Result_group: OFP time series of VOC classified by groups;
OFP_Result_group_mean: the average value and proportion of OFP of VOC according to major groups (sorted from large to small).

``` r
x = ofp(voc, unit = "ppbv")
View(x)
View(x[["MIR_Result"]])
View(x[["OFP_Result"]])
```


### Get OH Reactivity (koh)
----------
* #### Description

Theoretical values of the species' OH reaction constant kOH at 25 degrees were obtained from' Chemspider.com '. Value source: US Environmental Protection Agency’s EPISuite.

* #### Usage
``` r
koh(spec)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `spec`            |  species                          |          |The name of the species or CAS number are both acceptable.|

* #### Output

Output is the theoretical value of the species' OH reaction constant kOH at 25 degrees.

* #### Examples

``` r
koh("propane")
```


### Calculate OH reactivity (loh)
----------
* #### Description

Calculate OH radical reactivity (LOH) of VOC time series.  
CAS number corresponding to VOC species name was matched, and the VALUE of "OH reaction constant" (kOH) was matched by CAS number and used for time series calculation.  
Note: Missing values are automatically ignored when calculating (for example, summation).  
Note: By groups, biogenic VOC (BVOC) is listed separately from olefins. Biogenic species include: isoprene, alpha-pinene, beta-pinene.  
The kOH value comes from the US Environmental Protection Agency’s EPISuite "AOPWIN".  , which provides a theoretical reaction constant at 25 degrees Celsius.   
Note: If input VOC species contain M,P-xylene, it will be automatically divided into m-xylene and P-xylene evenly.  

* #### Usage
``` r
loh(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, colid = 1, wamg=FALSE)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of time series          |                            |                                      |
| `unit`            | unit for VOC data (micrograms per cubic meter or ppbv).Please fill in "ugm" or "ppbv" in quotation marks.| "ppbv"                     |  |
| `t`               | Temperature, in Degrees Celsius, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 25|                  |
| `p`               |Pressure, in kPa, used to convert data in micrograms per cubic meter to standard conditions (25 Degrees Celsius, 101.325 kPa).| 101.325|                         |
| `stcd`           | Does it output results of mass concentrations in standard conditions?        |FALSE      |                |
| `sortd`           | It determines whether the VOC species are sorted or not.      |TRUE       |Sequencing priority is: VOC groups, molecular mass, kOH value.Vocs include C ("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_organics ", "Other_Organic_Compounds", "Unknown").Among them, "Alkenes" does not include biogenic "BVOC".                              |
| `colid`           | column index for date-time        |1                           |                                      |
| `wamg`           | Should warnings be presented?      |FALSE                       | 

* #### Output

Output is a list containing 5 tables: 
KOH_Result: matched KOH value result;
LOH_Result: LOH time series of VOC by species;
LOH_Result_mean: the average value and proportion of LOH of VOC by species (sorted from large to small);
LOH_Result_group: LOH time series of VOC classified by groups;
LOH_Result_group_mean: the average value and proportion of LOH of VOC according to major groups (sorted from large to small).

* #### Examples

``` r
x = loh(voc, unit = "ppbv")
View(x)
View(x[["KOH_Result"]])
View(x[["LOH_Result"]])
```


### Format the historical data from OpenAQ (openaq)
----------
* #### Description

Format the historical data from openaq.org retrieved by AWS Athena.
How to download data? Please read https://medium.com/@openaq/how-in-the-world-do-you-access-air-quality-data-older-than-90-days-on-the-openaq-platform-8562df519ecd.

* #### Usage
``` r
openaq(df)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | dataframe of historical data from openaq.org with certain format|  |Need to have 5 species: "o3", "no2", "co", "pm25", "pm10".|

* #### Output

A list which contain a dataframe of formated OpenAQ data and a dataframe for time series of OpenAQ data.

* #### Examples

``` r 
#read data into R by using 'read_xlsx' function in 'readxl' pacakage
library(readxl)
#please use your path
path_for_file="xx.xlsx", col_types=c(rep("text",3), "numeric", rep("text",9))
aqidata<- read_xlsx(path_for_file,col_types = col_types)
#process data by using 'openaq' function in 'foqat' pacakage
x=openaq(aqidata)
#view results
View(x)
View(x[["aqidata"]])
View(x[["aqits"]])
```


### Calculate TUV in batch (tuv)
----------
* #### Description

There are online and offline versions of the TUV model, but both need to run on a daily basis (that means manually reset parameters for each day's simulation). This function runs TUV in batch by reading the time series for the parameters to be entered, and summarizes the results to the new dataframe.
Currently only mode 2 (mode that outputs the photolysis rate) is supported.
Before you use this function, please download [TUV executable for Windows](https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip).
* #### Usage
``` r
tuv(pathtuv, df, colid = 1)
```
* #### Arguments

| Variable name     |  Definition                       | Default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `pathtuv`         | path for parent folder of TUV executable for Windows|         |"G:/tuv5.3.1.exe"                     |
| `df`              | dataframe of the time series for the parameters to be entered|   |The date column must be included. |
| `colid`           | column index for date             |1                          |                                       |

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

## QUESTION OR FEEDBACK
Click the link to ask questions and give feedback: ☛ https://github.com/tianshu129/foqat/issues/new ☚   
Please send email to: tianshu129@163.com  

## DONATION

The project took a lot of time and effort.If this project is helpful to you, you are welcome to donate. Thank you very much:
<img src="./img/donation.png" width="200" height="200" alt="支付" align=center>
