## Overview

FOQAT is an R package designed for qucik analysis of atmospheric field observation data.

FOQAT stands for "Filed observation quick analysis toolkit".

## Installation

``` r
# Install from GitHub:
# install.packages("devtools")
devtools::install_github("tianshu129/foqat")
```

## Usage
Functions in foqat are listed below:

**dm8n**: calculate daily maximum-8-hour ozone.  
**koh**: search kOH value from 'chemspider.com'. Predicted data is generated using the US Environmental Protection Agency’s EPISuite.  
**t_rs**: resample time series and return complete time series with new time resolution.'  

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
