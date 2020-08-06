#' Format the historical data from openAQ retrieved by AWS Athena
#'
#' Format the historical data from openAQ retrieved by AWS Athena.
#'
#' @param df the historical data from openAQ retrieved by AWS Athena.
#' @return a list which contain a dataframe of formated OpenAQ data and a dataframe for time series of OpenAQ data.
#' @export
#' @importFrom stringr str_replace str_split_fixed

openaq<-function(df){
	utc_str=substr(aqidata[1,1], nchar(aqidata[1,1])-6, nchar(aqidata[1,1])-4)
	utc_str=str_replace(utc_str, "0", "")
	tz_str=paste0("Etc/GMT",as.character(utc_str),collapse = NULL)
	aqidata=transform(aqidata, utc = substr(date, 6, 28),local = substr(date, 38, 59))
	aqidata$local=as.POSIXct(as.character(aqidata$local),format="%Y-%m-%dT%H:%M:%S",tz = tz_str)
	aqidata$utc=as.POSIXct(as.character(aqidata$utc),format="%Y-%m-%dT%H:%M:%S",tz = "UTC")

	location=str_split_fixed(aqidata$coordinates, "\\{latitude\\=|\\, longitude\\=|\\}", 4)
	location=data.frame(location[,-c(1,4)])
	colnames(location)=c("latitude","longitude")
	averagingperiod=str_split_fixed(aqidata$averagingperiod, "\\{unit\\=|\\, value\\=|\\}", 4)
	averagingperiod=data.frame(averagingperiod[,-c(1,4)])
	colnames(averagingperiod)=c("unit","value")
	averagingperiod$averagingperiod <- paste(averagingperiod$value, averagingperiod$unit, sep=" ")

	aqidata=aqidata[,-c(1,8,9)]
	aqidata=data.frame(aqidata,averagingperiod,location)
	aqidata=aqidata[,c(11,12,1,3,4,15,2,16,17,seq.int(6,10,1))]

	o3=data.frame(local=aqidata[aqidata$parameter=="o3",2],o3=aqidata[aqidata$parameter=="o3",4])
	no2=data.frame(local=aqidata[aqidata$parameter=="no2",2],no2=aqidata[aqidata$parameter=="no2",4])
	co=data.frame(local=aqidata[aqidata$parameter=="co",2],co=aqidata[aqidata$parameter=="co",4])
	so2=data.frame(local=aqidata[aqidata$parameter=="so2",2],so2=aqidata[aqidata$parameter=="so2",4])
	pm25=data.frame(local=aqidata[aqidata$parameter=="pm25",2],pm25=aqidata[aqidata$parameter=="pm25",4])
	pm10=data.frame(local=aqidata[aqidata$parameter=="pm10",2],pm10=aqidata[aqidata$parameter=="pm10",4])

	final_df=merge(x = o3, y = no2, by = "local", all=TRUE)
	final_df=merge(x = final_df, y = co, by = "local", all=TRUE)
	final_df=merge(x = final_df, y = so2, by = "local", all=TRUE)
	final_df=merge(x = final_df, y = pm25, by = "local", all=TRUE)
	final_df=merge(x = final_df, y = pm10, by = "local", all=TRUE)

	results<-list(aqidata=aqidata, aqits=final_df)
	return(results)
}
