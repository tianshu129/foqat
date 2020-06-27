#' Calculate ozone formation potential
#'
#' Calculate ozone formation potential of VOC time series.
#'
#'Calculate ozone formation potential of VOC time series. Returns MIR dataframe, OFP dataframe.
#'
#' @param df dataframe contains time series.
#' @param colid column index of datetime in dataframe.
#' @return  dataframes for MIR and OFP.
#' @export
#' @importFrom utils URLencode
#' @importFrom xml2 read_html

ofp <- function(df, colid = 1){

#set colid
if(colid != 1){
	df[,c(1,colid)] = df[,c(colid,1)]
	colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
}

#get VOC name by colnames of df, build name_df
name_df = data.frame(name = colnames(df)[2:ncol(df)],CAS = NA, Source = NA, Matched_Name = NA, MIR = NA, stringsAsFactors = FALSE)

#search VOC name to get CAS Number from different sources, add cas, sources, mathed_name to name_df
##firstly by NIST
for( i in 1:nrow(name_df)){  #Dipropylene glycol methyl ether acetate isomer #1有问题！！！！
	str <- name_df[i,1]
	str <- URLencode(str)
	url=paste(c("https://webbook.nist.gov/cgi/cbook.cgi?Name=", str,"&Units=SI"), collapse='')
	download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
	web <- read_html("scrapedpage.html")
	result_test<-web%>%html_nodes("h1")%>%html_text()
	if(result_test[2] == "Name Not Found"){
		name_df[i,2]="Name Not Found"
		name_df[i,3]=""
	}else if(result_test[2] == "Search Results"){
		name_df[i,2]="More than 1 result"
		name_df[i,3]=""
	}else{
		result_test<-web%>%html_nodes("li")%>%html_text()
		result_test<-strsplit(result_test[21], ": ")
		name_df[i,2]=result_test[[1]][2]
		name_df[i,3]="NIST"
	}
}

#match mir by different sources
##by NIST
a=lapply(name_df$CAS[which(name_df$Source=="NIST")], function(i) grep(i, datacas$`CAS`))
a=unlist(lapply(a,function(x) if(identical(x,integer(0))) ' ' else x))
name_df$MIR[which(name_df$Source=="NIST")] = datacas$New[as.numeric(a)]
name_df$Matched_Name[which(name_df$Source=="NIST")] = datacas$Description[as.numeric(a)]


for(k in which(name_df$Source=="")){
tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
tar=tolower(name_df$name[k])
df_null=data.frame(datacas[tarlist %in% tar,])
if(nrow(df_null)!=0){
	name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
	name_df$CAS[as.numeric(k)] = df_null$CAS[1]
	name_df$MIR[as.numeric(k)] = df_null$New[1]
	name_df$Source[as.numeric(k)] = "Carter paper 2010"
}
}

#multiple df with MIR in name_df
ofp_df=df
ofp_df[,2:ncol(ofp_df)]=data.frame(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$MIR)[x-1]))

#results
results <-list(MIR_Result = name_df, OFP_Result = ofp_df)
return(results)
}











