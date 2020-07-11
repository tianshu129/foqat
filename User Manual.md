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

* [数据统计](#数据统计-statdf)

* [数据重采样 (会自动补齐时间，包含风数据处理）](#数据重采样-trs)

* [计算平均变化 (包含风数据处理）](#平均变化-avri)

* [计算臭氧生成潜势（OFP）](#臭氧生成潜势-ofp)

* [计算最大八小时臭氧](#最大八小时臭氧-dm8n)

* [获取OH反应活性](#OH反应活性-koh)

* [批量计算对流层紫外可见(TUV)辐射模型](#对流层紫外可见TUV辐射模型批量计算-tuv)

包中包含了4个随机样本数据集（时长5天），用于演示功能:

* 空气质量数据集aqi

* 气象数据集met

* 挥发性有机物数据集voc

* 配置tuv的示范数据集setup_tuv


## INSTALLATION OF R & RSTUDIO
Please install the R program first, then install the R integrated development platform Rstudio (Desktop version).Then open Rstudio and enter the code from the console at the bottom right of the interface.Please click on the link: 
[R](https://cloud.r-project.org/)  
[rstudio-desktop](https://rstudio.com/products/rstudio/#rstudio-desktop)  

## BREIF KNOWLEDGE OF R

### R的函数 
----------
R语言中，操作依赖于函数。 

函数存在于R包中。 

想要运行函数需要先调用已安装的包。例如，以下语句将调用readxl包，该包含有用于读取excel文件的函数：  
``` r
library(readxl)
```

默认R程序只自带一些基础函数。需要额外安装包来获取所需函数。R包分为官方包和非官方包。安装方式分为在线和离线。以下介绍在线安装方式：    
官方包安装方式：  
``` r
install.packages("readxl")
```
非官方包安装方式（通过官方包‘remotes’中的‘install_github’函数从github上安装非官方包）：  
``` r
#安装官方包remotes
install.packages("remotes")
#调用其中的‘install_github’函数安装非官方包
remotes::install_github("tianshu129/foqat")
```

函数由“函数名（参数1，参数2，···）”构成。不同参数之间用逗号隔开。函数中的符号都是英文符号。可将运行结果赋值给任意变量。例如以下代码将生成一个包含3个1的向量集：
``` r
x=rep(1,3)
View(x)
```

此外， 需要特别指出的是，一些函数的参数有默认值。如果有默认值，则函数用法中会列出默认值。例如“函数A（参数1，参数2=参数2的默认值，···）”，则函数A中，参数1无默认值，参数2有默认值。  
如果默认值符合你的需求，则无需列出该参数。例如，以下两句函数得到的结果是一样的（statdf参数中，n有默认值2，假设你要设置的n也是2）：  
``` r
statdf(x, n = 2)
statdf(x)
```

### 数据读写 
----------
R有数据读写有很多方式。此处讲解最常用的方式。
可以将数据放在xlsx文件中，并通过readxl包中的read_xlsx函数将数据读入R中的任意变量。例如：
``` r
library(readxl)
#此处跳过了很多默认参数，第1个参数为文件路径，第2个参数为每一列的数据类型，第3个参数将na值读取为空，第4个参数是sheet表号。读取数据后赋值给变量df。
df <- read_xlsx("E:/Users/Chen/Desktop/input.xlsx", col_types = c("date",rep("numeric",7)), na = "", sheet = 1)
```

可以采用内置函数write.csv将数据写入csv文件中，例如：
``` r
#此处跳过了很多默认参数，第1个参数为要输出的数据变量，第2个参数为输出路径，第3个参数设置为不输出列号。
write.csv(result,"E:/Users/Chen/Desktop/tuv_result.csv",row.names=F)
```


## INSTALLATION OF FOQAT
本包当前不是官方包，需要安装官方包‘remotes’。然后用"remotes"中的‘install_github’函数在线安装本包。
``` r
#安装官方包‘remotes’
install.packages("remotes")
#调用其中的‘install_github’函数安装本包
remotes::install_github("tianshu129/foqat")
```


## FUNCTIONS AND EXAMPLES

### Summary of data (statdf) 
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


### 数据重采样 trs
----------
* #### Description

对时间序列进行分辨率重采样（可处理风向、风速数据）。

* #### Usage
``` r
trs(df, bkip, colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | 时间序列的数据框                    |                            |                                     |
| `bkip`            | 重采样的时间区间长度                |                            | '1 hour'                             |
| `colid`           | 时间列的列号                       |                            |                                      |
| `st`              | 开始时间                           | NULL                       | 默认取时间序列开始时间。               |
| `et`              | 结束时间                           | NULL                       | 默认取时间序列结束时间。               
| `na.rm`           | 是否排除NA值                       | TRUE                       |                                      |
| `wind`            | 是否包含风向风速数据                | FALSE                      |                                      |
| `coliws`          | 风速列的列号                       | 2                          |                                      |
| `coliwd`          | 风向列的列号                       | 3                          | 风向为度数（dgree)                    |

* #### Output

输出一个数据框，内容为新时间分辨率的时间序列。

* #### Examples

``` r
x = trs(met, bkip = "1 hour", colid = 1, st = "2017-05-01 00:00:00", et = NULL, na.rm = TRUE, wind = TRUE, coliws = 4, coliwd = 5)
View(x)
```


### 平均变化 avri
----------
* #### Description

计算时间序列的平均变化（可以但不限于：日均变化、月均变化、年均变化）。

* #### Usage
``` r
avri(df, bkip, mode = "recipes", value = "day", colid = 1, st = NULL, et = NULL, na.rm = TRUE, wind = FALSE, coliws = 2, coliwd = 3)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | 时间序列的数据框                    |                            |                                      |
| `bkip`            | 基础时间区间长度                    |                            | 计算小时分辨率的平均日变化就写'1 hour'。|
| `mode`            | 计算平均变化的模式                  |"recipes"                   | 可选模式为："recipes", "ncycle", "custom"。"recipes"代表内置方案。"ncycle"代表用个数表示每个周期。"custom"代表选取数据框中的某列作为计算平均变化的参考。|
| `value`           | 模式所需的参数值                    |"day"                       | 模式"recipes"对应三种数值："day", "week", "month"。"day"代表天为周期，小时为分辨率。"week"代表周为周期，天为分辨率。"month"代表月为周期，天为分辨率。  模式"ncycle"对应数值型value，表示每个周期的元素个数。  模式"custom"对应数值型value，表示数据框中的列号。|
| `colid`           | 时间列的列号                       |                            |                                      |
| `st`              | 开始时间                           | NULL                       | 默认取时间序列开始时间。需要注意的是，当模式用"ncycle"或"custom"时，起始时间决定了平均变化的结果中第一个要素的开始时间。例如：数据第一个时间为"2010-05-01 12:00:00"， 分辨率为1小时，模式用"ncycle"，”value"用24，则平均变化结果是从12点开始的日均变化。|
| `et`              | 结束时间                           | NULL                       | 默认取时间序列结束时间。                 |
| `na.rm`           | 是否排除NA值                       | TRUE                       |                                      |
| `wind`            | 是否包含风向风速数据                | FALSE                      |                                      |
| `coliws`          | 风速列的列号                       | 2                          |                                      |
| `coliwd`          | 风向列的列号                       | 3                          |  风向为度数（dgree)                    |

* #### Output

输出为一个数据框，内容为平均变化。第一列为周期内的序号，第二列开始为平均变化数据。需要注意的是，当模式用"ncycle"或"custom"时，起始时间决定了平均变化的结果中第一个要素的开始时间。例如：数据第一个时间为"2010-05-01 12:00:00"， 分辨率为1小时，模式用"ncycle"，”value"用24，则平均变化结果的是从12点开始的日均变化（24个数）。 

* #### Examples

``` r
x = avri(met, bkip = "1 hour", mode = "recipes", value = "day", colid = 1, st = "2017-05-01 00:00:00", et = NULL, na.rm = TRUE, wind = TRUE, coliws = 4, coliwd = 5)
View(x)
View(x[["df_average"]])
View(x[["df_sd"]])
```


### 臭氧生成潜势 ofp
----------
* #### Description
计算VOC时间序列的“臭氧生成潜势”（OFP）。  
搜寻匹配CAS号，通过CAS号查询对应“最大增量反应活性”（MIR）值，并用于时间序列计算。  
MIR值来“Carter, W. P. (2009). Updated maximum incremental reactivity scale and hydrocarbon bin reactivities for regulatory applications. California Air Resources Board Contract, 2009, 339”（修改于 January 28, 2010）。
* #### Usage
``` r
ofp(df, unit = "ugm", t = 25, p = 101.325, colid = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | VOC时间序列的数据框                |                            |                                      |
| `unit`            | VOC数据的单位（微克每立方米或者ppb)  | "ugm"                     | 填写"ugm"或者"ppb"(带英文引号）。       
| `t`               | 温度，单位k，用于从ppb换算至微克每立方米| 25                      |                                      |
| `p`               | 压强，单位kPa，用于从ppb换算至微克每立方米| 101.325               |                                      |
| `colid`           | 时间列的列号                       |                            |                                      |

* #### Output

输出1个列表，其中包含2个表：MIR值匹配结果、OFP时间序列。

* #### Examples

``` r
x = ofp(voc, unit = "ppb", t = 25, p = 101.325, colid = 1)
View(x)
View(x[["MIR_Result"]])
View(x[["OFP_Result"]])
```


### 最大八小时臭氧 dm8n
----------
* #### Description

计算每日最大八小时臭氧。

* #### Usage
``` r
dm8n(df, colid = 1, starthour = 0, endhour = 16, na.rm = TRUE, outputmode = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `df`              | 臭氧时间序列的数据框                     |                       | 可以同时计算多个站点，输入多列即可。    |
| `colid`           | 时间列的列号                       |                            |                                      |
| `starthour`       | 平均八小时臭氧的起点时间            | 0                          |                                      |
| `endhour`         | 平均八小时臭氧的终点时间            | 16                         |16就是平均16-23点的八小时臭氧。          |
| `na.rm`           | 是否排除NA值                       | TRUE                       |                                      |
| `outputmode`      | 输出结果的格式                     | 1                           |填写1或2的结果见“输出”。               |

* #### 输出

取决于函数中的参数`outputmode`。填1只输出1个表：最大八小时臭氧。填2输出1个列表，其中包含3个表：八小时臭氧、每个计算区间的数据个数统计、最大八小时臭氧。

* #### Examples

``` r 
x = dm8n(aqi[,c(1,6)], colid = 1, starthour = 0, endhour = 16, na.rm = TRUE, outputmode = 2)
View(x)
View(x[["D8"]])
View(x[["D8_count"]])
View(x[["DMAX8"]])
```


### OH Reactivity (koh)
----------
* #### Description

从‘chemspider.com’查询物种在25度条件下的OH反应常数kOH的理论值。数值来源为US Environmental Protection Agency’s EPISuite。

* #### Usage
``` r
koh(spec)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `spec`            | 物种                               |                            |输入物种名称或者CAS号均可。            |

* #### Output

输出物种在25度条件下的OH反应常数kOH的理论值。

* #### Examples

``` r
koh("propane")
```


### 对流层紫外可见(TUV)辐射模型批量计算 tuv
----------
* #### Description

TUV模型有在线版本和离线版本，但是都需要按天跑（也就是每跑一天重新设置一次参数）。该功能通过读取拟输入的参数及其数值的时间序列批量运行TUV，并汇总结果至新数据框。
当前仅支持模式2（输出光解速率的模式）。
使用该函数之前，请先下载TUV的windows程序[TUV executable for Windows](https://www2.acom.ucar.edu/sites/default/files/modeling/tuv5.3.1.exe_.zip)。
* #### Usage
``` r
tuv(pathtuv, df, colid = 1)
```
* #### Arguments

| variable name     |  definition                       | default                    | Example values/remarks               |
| ------------------| ----------------------------------|----------------------------|--------------------------------------|
| `pathtuv`         | TUV的windows程序的母文件夹路径      |                            |"G:/tuv5.3.1.exe"                     |
| `df`              | 拟输入的参数及其数值的时间序列的数据框|                            |必须包含日期列。                       |
| `colid`           | 时间列的列号                       |                            |                                      |

* #### Output

输出一个数据框。第一列为时间。第二列为太阳高度角。第三列开始为各个反应的光解速率。依次为（单位为s-1）:  
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
