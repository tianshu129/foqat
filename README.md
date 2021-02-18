# FOQAT <img src="https://s1.ax1x.com/2020/08/31/dLqtdf.png" align="right" width="120" />

![Language](https://img.shields.io/badge/Language-R-blue.svg) [![GPLv3 license](https://img.shields.io/badge/License-GPLv3-success.svg)](http://perso.crans.org/besson/LICENSE.html)
![Version](https://img.shields.io/badge/Version-1.6.1-important) 
 
## Overview

The FOQAT is an R package designed for quick processing and analysis of atmospheric data measured from field observation and air pollution. And the functions for time series analysis could also applied to time serires of any other fileds.  

The FOQAT stands for "Field observation quick analysis toolkit".

The FOQAT package is developed and maintained by Chen Tianshu from Professor Xue Likun's research group of Environmental Research Institute of Shandong University.  

For getting start or detail usage, please visit: [user manual](https://github.com/tianshu129/foqat/blob/master/UserManual.md) or [说明手册](https://github.com/tianshu129/foqat/blob/master/%E8%AF%B4%E6%98%8E%E6%89%8B%E5%86%8C.md) .  

If FOQAT is used to support a scientific publication, please cite the following reference:

>*Tianshu Chen: Field Observation Quick Analysis Toolkit (FOQAT), available at: https://github.com/tianshu129/foqat (last access: 12 July 2020), 2020.* 

In addition, please send an email to [me](mailto:tianshu129@163.com) so that the paper may be added to my 'List of Publications Using FOQAT'.  

As far as I know, FOQAT has users from: 
* Chinese Research Academy of Environmental Sciences (CRAES)
* Chinese Academy of Meteorological Sciences (CAMS)
* Research Center for Eco-Environmental Sciences (RCEES) for Chinese Academy of Sciences (CAS)
* Anhui Institute of Optics and Fine Mechanics (AIOFM) for Chinese Academy of Sciences (CAS)
* Peking University (PKU)
* City University of Hong Kong (CityU)
* The Hong Kong Polytechnic University (PolyU)
* Shandong University (SDU)

## Installation 

``` r
# Install from GitHub:
install.packages("remotes")
remotes::install_github("tianshu129/foqat")
```

## Functions
Functions in foqat are listed below:  
**statdf**: summary each variable of dataframe into: mean, sd, min, max, percentiles (25%, 50%, 75%).  
**trs**: resample time series and return complete time series with new time resolution.  
**avri**: calculate average of variation of time series.   
**anylm**: analyse linear regression for time series in batch.  
**dm8n**: calculate daily maximum-8-hour ozone.    
**vocct**: convert and analyse VOC concentrations   
**ofp**: calculate ozone formation potential (OFP) of VOC time series.  
MIR values are refered from "Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339" (Revised January 28, 2010).  
**koh**: search kOH value from 'chemspider.com'. Predicted data is generated using the US Environmental Protection Agency’s EPISuite.  
**loh**: calculate OH reactivity (LOH) of VOC time series. kOH value is generated using the US Environmental Protection Agency’s EPISuite "AOPWIN".  
**openaq**: process the historical data from openaq.org retrieved by AWS Athena.  
**tuv**: run offline batch calculation of [TROPOSPHERIC ULTRAVIOLET AND VISIBLE (TUV) RADIATION MODEL](https://www2.acom.ucar.edu/modeling/tropospheric-ultraviolet-and-visible-tuv-radiation-model).  
Currently, this function only support output of photolysis rate coefficients (J-values). 
If you want to know detail about each function, please type ?functionname in R. Below is an example:

``` r
library(foqat)
?dm8n
```

## QUESTION OR FEEDBACK
Click the link to ask questions and give feedback: ☛ https://github.com/tianshu129/foqat/issues/new ☚   
Please send email to: tianshu129@163.com  

## Link
[![Personal Academic Site](https://img.shields.io/badge/AcademicSite-Tianshu%20Chen-00CCBB)](https://tianshu129.github.io/)  
[![ResearchGate](https://img.shields.io/badge/ResearchGate-Tianshu%20Chen-00CCBB)](https://www.researchgate.net/profile/Tianshu_Chen)   
[![Google Scholar](https://img.shields.io/badge/GoogleScholar-Tianshu%20Chen-red)](https://scholar.google.com/citations?user=VfnzOQgAAAAJ&hl=en)  
[![Twitter](https://img.shields.io/twitter/follow/tichpi.svg?style=social&label=@tichpi)](https://twitter.com/tichpi)  
[![zhihu-shield]][zhihu]  
--------------------------------
[zhihu]:https://www.zhihu.com/people/tichpi "我的知乎，欢迎关注"
[zhihu-shield]:https://img.shields.io/badge/dynamic/json?color=0084ff&logo=zhihu&label=TichPi&query=%24.data.totalSubs&url=https%3A%2F%2Fapi.spencerwoo.com%2Fsubstats%2F%3Fsource%3Dzhihu%26queryKey%3Dtichpi

## Donation

The project took a lot of time and effort. If this project is helpful to you, you are welcome to donate. Thank you very much:  
Wechat Pay  
<img src="img/donation.png" width="200" height="200" alt="支付" align=center>
