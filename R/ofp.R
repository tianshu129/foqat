#' Calculate ozone formation potential
#'
#' Calculate ozone formation potential of VOC time series.
#'
#'Calculate ozone formation potential of VOC time series. Returns MIR dataframe, OFP dataframe.
#'
#' @param df dataframe contains time series.
#' @param colid column index of datetime in dataframe.
#' @param unit unit for VOC concentration. A character vector from these options: "ugm" or "ppb". "ugm" means ug/m3. "ppb" means part per billion volumn.
#' @param t temperature for conversions. Unit for t is "degrees Celsius". By default, t equals to 25 degrees Celsius.
#' @param p pressure for conversions. Unit for p is "kPa". By default, p equals to 101.325 kPa.
#' @return  dataframes for MIR and OFP.
#' @export
#' @examples
#' ofp(voc, unit = "ppb")
#' @importFrom utils URLencode
#' @importFrom xml2 read_html

ofp <- function(df, unit = "ugm", t = 25, p = 101.325, colid = 1){

  #set colid
  if(colid != 1){
    df[,c(1,colid)] = df[,c(colid,1)]
    colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
  }

  #get VOC name by colnames of df
  #if read from xlsx, replace "X" and "."
  colnm_df = colnames(df)[2:ncol(df)]
  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
  chemicalnames = gsub("\\.", "-", chemicalnames)
  #build name_df
  name_df = data.frame(name = chemicalnames,CAS = NA, Source = NA, Matched_Name = NA, MIR = NA, MW = NA, stringsAsFactors = FALSE)

  #search VOC name to get CAS Number from different sources, add cas, sources, mathed_name to name_df
  ##firstly by NIST
  for( i in 1:nrow(name_df)){
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
  name_df$MW[which(name_df$Source=="NIST")] = datacas$MWt[as.numeric(a)]

  for(k in which(name_df$Source=="")){
    tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
    tar=tolower(name_df$name[k])
    df_null=data.frame(datacas[tarlist %in% tar,])
    if(nrow(df_null)!=0){
      name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
      name_df$CAS[as.numeric(k)] = df_null$CAS[1]
      name_df$MIR[as.numeric(k)] = df_null$New[1]
      name_df$Source[as.numeric(k)] = "Carter paper 2010"
      name_df$MW[as.numeric(k)] = df_null$MWt[1]
    }
  }

  #multiple df with MIR in name_df
  ofp_df=df
  if(unit=="ugm"){
    ofp_df[,2:ncol(ofp_df)] = data.frame(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$MIR)[x-1]))
    #results
    results <- list(MIR_Result = name_df, OFP_Result = ofp_df)
    return(results)
  }else if(unit=="ppb"){
    r = 22.4*(273.15+t)*101.325/(273.15*p)
    ofp_df[,2:ncol(ofp_df)] = data.frame(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$MW*name_df$MIR/r)[x-1]))
    #results
    results <- list(MIR_Result = name_df, OFP_Result = ofp_df)
    return(results)
  }else{
    print("unit error")
  }
}
