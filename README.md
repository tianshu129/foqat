## Overview

FOQAT is an R package designed for qucik analysis of atmospheric (especially for chemistry) field observation and air pollution data.

FOQAT stands for "Filed observation quick analysis toolkit".

## Installation

``` r
# Install from GitHub:
# install.packages("devtools")
devtools::install_github("tianshu129/foqat")
```
If fail to run 'install.packages("devtools")', please change primary CRAN repository to nearby location.  
Tools->Global options->Packages->Primary CRAN repository  

If fail to run 'devtools::install_github("tianshu129/foqat")', please enable TLS 1.2 in IE 10.  

## Usage
Functions in foqat are listed below:

**dm8n**: calculate daily maximum-8-hour ozone.  
**koh**: search kOH value from 'chemspider.com'. Predicted data is generated using the US Environmental Protection Agency’s EPISuite.  
**trs**: resample time series and return complete time series with new time resolution.   
**tuv**: run offline batch calculation of [TROPOSPHERIC ULTRAVIOLET AND VISIBLE (TUV) RADIATION MODEL](https://www2.acom.ucar.edu/modeling/tropospheric-ultraviolet-and-visible-tuv-radiation-model).  
Currently, this function only support output of photolysis rate coefficients (J-values). 
Columns of photolysis rate coefficients (s-1):  
1 = O<sub>3</sub> -> O<sub>2</sub> + O<sub>1D</sub>  
2 = H<sub>2</sub>O<sub>2</sub> -> 2 OH  
3 = NO<sub>2</sub> -> NO + O<sub>3P</sub>  
4 = NO<sub>3</sub> -> NO + O<sub>2</sub>   
5 = NO<sub>3</sub> -> NO<sub>2</sub> + O<sub>3P</sub>  
6 = CH<sub>2</sub>O -> H + HCO  
7 = CH<sub>2</sub>O -> H<sub>2</sub> + CO  
Please download [TUV executable for Windows](https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip) before you run this function.  

If you want to know detail about each function, please type ?functionname in R. Below is an example:

``` r
library(foqat)
?dm8n
```

## Getting help

Please contact: tianshu129@163.com

## Link

知乎 [![zhihu-shield]][zhihu]
--------------------------------
[zhihu]:https://www.zhihu.com/people/chen-xiao-tian-92-92 "我的知乎，欢迎关注"
[zhihu-shield]:https://img.shields.io/badge/dynamic/json?color=0084ff&logo=zhihu&label=TichPi&query=%24.data.totalSubs&url=https%3A%2F%2Fapi.spencerwoo.com%2Fsubstats%2F%3Fsource%3Dzhihu%26queryKey%3Dchen-xiao-tian-92-92
