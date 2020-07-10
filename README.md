## Overview

FOQAT is an R package designed for quick analysis of atmospheric (especially for chemistry) field observation and air pollution time series data. And the functions for time series analysis could also applied to time serires of any other fileds. 

FOQAT stands for "Filed observation quick analysis toolkit".

Twitter: [@tichpi](https://twitter.com/tichpi)

## Installation

``` r
# Install from GitHub:
install.packages("remotes")
remotes::install_github("tianshu129/foqat")
```

## Usage
Functions in foqat are listed below:  
**statdf**: summary each variable of dataframe into: mean, sd, min, pecentag evalue (25%, 50%, 75%), max.  
**trs**: resample time series and return complete time series with new time resolution.  
**avri**: calculate average of variation of time series.  
**ofp**: calculate ozone formation potential (OFP) of VOC time series.  
MIR values are refered from "Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339" (Revised January 28, 2010).  
**dm8n**: calculate daily maximum-8-hour ozone.  
**koh**: search kOH value from 'chemspider.com'. Predicted data is generated using the US Environmental Protection Agency’s EPISuite.  
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
[![Twitter](https://img.shields.io/twitter/follow/tichpi.svg?style=social&label=@tichpi)](https://twitter.com/tichpi)  
知乎 [![zhihu-shield]][zhihu]
--------------------------------
[zhihu]:https://www.zhihu.com/people/chen-xiao-tian-92-92 "我的知乎，欢迎关注"
[zhihu-shield]:https://img.shields.io/badge/dynamic/json?color=0084ff&logo=zhihu&label=TichPi&query=%24.data.totalSubs&url=https%3A%2F%2Fapi.spencerwoo.com%2Fsubstats%2F%3Fsource%3Dzhihu%26queryKey%3Dchen-xiao-tian-92-92

## Donation

The project took a lot of time and effort. If this project is helpful to you, you are welcome to donate. Thank you very much:  
Wechat Pay  
<img src="./img/donation.jpg" width="200" height="200" alt="支付" align=center>
