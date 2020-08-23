#' Calculate OH reactivity
#'
#' Calculate OH reactivity of VOC time series in 25 degree celsius.
#'
#' The CAS number was matched for each VOC speices (from column name), and the
#' OH Rate Constant was matched through the CAS number and used for time series calculation. \cr
#' The OH Rate Constant comes from 'AopWin v1.92' in 25 degree celsius.
#'
#' @param df dataframe contains time series.
#' @param colid column index for date-time. The default value is 1.
#' @param unit unit for VOC concentration. A character vector from these options: "ugm" or "ppbv". "ugm" means ug/m3. "ppbv" means part per billion volumn.
#' @param t Temperature, in Degrees Celsius, used to convert data in 
#' micrograms per cubic meter to standard conditions 
#' (25 Degrees Celsius, 101.325 kPa). By default, t equals to 25 Degrees Celsius.
#' @param p Pressure, in kPa, used to convert data in micrograms per cubic meter 
#' to standard conditions (25 Degrees Celsius, 101.325 kPa). 
#' By default, p equals to 101.325 kPa.
#' @param sortd logical value. It determines whether the VOC species
#' are sorted or not. By default, sortd has value "TRUE".
#' If TRUE, VOC species in time series will be arranged according to VOC group,
#'  relative molecular weight, and OH Rate Constant.
#' @param colid column index for date-time. The default value is 1.
#' @param wamg logical. Should warnings be presented? The default vaule is FALSE.
#' @return  a list contains 13 tables:
#' Con_ugm: time series of VOC mass concentration by species;
#' Con_ugm_mean: the average mass concentration and proportion of VOC by species (sorted from large to small);
#' Con_ugm_group: time series of VOC mass concentration classified by groups;
#' Con_ugm_group_mean: the average value and proportion of VOC mass concentration (sorted from large to small) according to major groups;
#' Con_ppbv: time series of VOC volume concentration by species;
#' Con_ppbv_mean: the average volume concentration and proportion of VOC by species (sorted from large to small);
#' Con_ppbv_group: time series of VOC volume concentration according to major groups;
#' Con_ppbv_group_mean: VOC volume concentration average and proportion (sorted from large to small) according to major groups;
#' KOH_Result: matched KOH value result;
#' LOH_Result: LOH time series of VOC by species;
#' LOH_Result_mean: the average value and proportion of LOH of VOC by species (sorted from large to small);
#' LOH_Result_group: LOH time series of VOC classified by groups;
#' LOH_Result_group_mean: the average value and proportion of LOH of VOC according to major groups (sorted from large to small).
#'
#' @export
#' @examples
#' loh(voc)
#' @importFrom utils URLencode
#' @importFrom xml2 read_html

loh <- function(df, unit = "ppbv", t = 25, p = 101.325, sortd =TRUE, colid = 1, wamg=FALSE){

  #suppress warnings temporarily?
  if(wamg==FALSE){options(warn=-1)}

  #set colid
  if(colid != 1){
    df[,c(1,colid)] = df[,c(colid,1)]
    colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
  }

  #In case df is not a dataframe.
  temp_col_name <- colnames(df)
  df <- data.frame(df,stringsAsFactors = FALSE)
  colnames(df) <- temp_col_name

  #get VOC name by colnames of df
  #if read from xlsx, replace "X" and "."
  colnm_df = colnames(df)[2:ncol(df)]
  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
  chemicalnames = gsub("\\.", "-", chemicalnames)
  #if i-
  chemicalnames = gsub("\\i-", "iso-", chemicalnames)

  #build name_df
  name_df = data.frame(name = chemicalnames,CAS = NA, Source = NA, Matched_Name = NA, koh = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

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
      name_df[i,3]=NA
    }else if(result_test[2] == "Search Results"){
      name_df[i,2]="More than 1 result"
      name_df[i,3]=NA
	}else if(grepl("structure unspecified",result_test[2])){
	  name_df[i,2]="structure unspecified in NIST"
	  name_df[i,3]=NA
    }else{
      result_test<-web%>%html_nodes("li")%>%html_text()
      result_test<-strsplit(result_test[21], ": ")
      name_df[i,2]=result_test[[1]][2]
      name_df[i,3]="NIST"
    }
  }

  #match koh by different sources
  ##get CAS from NIST, match name by CAS
  a=lapply(name_df$CAS[which(name_df$Source=="NIST"&!is.na(name_df$CAS))], function(i) grep(i, datacas$CAS))
  a=unlist(lapply(a,function(x) if(identical(x,integer(0))) ' ' else x))
  name_df$koh[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacas$koh[as.numeric(a)]
  name_df$Matched_Name[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacas$Description[as.numeric(a)]
  name_df$MW[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacas$MWt[as.numeric(a)]
  name_df$Group[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacas$Group[as.numeric(a)]


  #if it is matched by CAS in NIST and matched by name in Carter paper, but it doesn't have CAS in Carter paper.
  for(k in which(!is.na(name_df$Source)&is.na(name_df$koh))){
    tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
    tar=tolower(name_df$name[k])
    df_null=data.frame(datacas[tarlist %in% tar,])
    if(nrow(df_null)!=0){
      name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
      #name_df$CAS[as.numeric(k)] = df_null$CAS[1]
      name_df$koh[as.numeric(k)] = df_null$koh[1]
      name_df$Source[as.numeric(k)] = "CAS is found in NIST. But it only has name in Carter paper 2010"
      name_df$MW[as.numeric(k)] = df_null$MWt[1]
	  name_df$Group[as.numeric(k)] = df_null$Group[1]
    }
  }

  #if it isn't found in NIST, but its name is matched by Carter paper.
  for(k in which(is.na(name_df$Source))){
    tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
    tar=tolower(name_df$name[k])
    df_null=data.frame(datacas[tarlist %in% tar,])
    if(nrow(df_null)!=0){
      name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
      name_df$CAS[as.numeric(k)] = df_null$CAS[1]
      name_df$koh[as.numeric(k)] = df_null$koh[1]
      name_df$Source[as.numeric(k)] = "Carter paper 2010"
      name_df$MW[as.numeric(k)] = df_null$MWt[1]
	  name_df$Group[as.numeric(k)] = df_null$Group[1]
    }
  }

  #set GROUP to Unknown for NA group
  name_df$Group[is.na(name_df$Group)] = "Unknown"

  #set GROUP to BVOC for BVOC group
  name_df$Group[name_df$CAS %in% c('80-56-8','127-91-3','78-79-5')] = "BVOC"

  #raw_order
  name_df$raw_order = seq.int(nrow(name_df))

  #set order for voc species in df and name_df
  if(sortd==TRUE){
	  #order by 2 columns
	  name_df$Group <- factor(name_df$Group, levels = c("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown"))
	  name_df = name_df[with(name_df, order(Group, MW, koh)), ]
	  df[,2:ncol(df)]=df[,name_df$raw_order+1]
	  colnames(df)[2:ncol(df)]=colnames(df)[name_df$raw_order+1]
  }

  #set concentration df, multiple df with koh in name_df
  loh_df = df
  r = 22.4*(273.15+t)*101.325/(273.15*p)
  r2 = (273.15+t)*101.325/(273.15*p)
  Avogadro = 6.022e23
  if(unit=="ugm"){
    Con_ugm = df
	Con_ppbv = Con_ugm
	Con_ppbv[,2:ncol(Con_ugm)] = data.frame(sapply(2:ncol(Con_ugm),function(x) Con_ugm[,x]*as.numeric(r/name_df$MW)[x-1]))
	loh_df[,2:ncol(loh_df)] = data.frame(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$koh*Avogadro*1e-12/(name_df$MW*r2))[x-1]))
  }else if(unit=="ppbv"){
    Con_ppbv = df
	Con_ugm = Con_ppbv
	Con_ugm[,2:ncol(Con_ppbv)] = data.frame(sapply(2:ncol(Con_ppbv),function(x) Con_ppbv[,x]*as.numeric(name_df$MW/24.45016)[x-1]))
    loh_df[,2:ncol(loh_df)] = data.frame(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$koh*Avogadro*1e-12/24.45016)[x-1]))
  }else{
    print("unit error")
  }

  #vector of group names
  gn_list = c("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown")

  #generate group df
  Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  loh_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)

  #sum up columns
  for(gn in 1:length(gn_list)){
	gn_sub_index = which(name_df$Group == gn_list[gn])
	if(length(gn_sub_index)!=0){
		if(length(gn_sub_index)==1){
			Con_ppbv_group[,gn+1]=Con_ppbv[,gn_sub_index+1]
			Con_ugm_group[,gn+1]=Con_ugm[,gn_sub_index+1]
			loh_df_group[,gn+1]=loh_df[,gn_sub_index+1]
		}else{
			Con_ppbv_group[,gn+1]=rowSums(Con_ppbv[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ppbv[,gn_sub_index+1]))
			Con_ugm_group[,gn+1]=rowSums(Con_ugm[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ugm[,gn_sub_index+1]))
			loh_df_group[,gn+1]=rowSums(loh_df[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(loh_df[,gn_sub_index+1]))
		}
	}
  }

  #Con_ugm_mean
  Con_ugm_mean=data.frame(species=row.names(statdf(Con_ugm)[-1,]),mean=as.numeric(as.character(statdf(Con_ugm,n = 6)[-1,1])))
  Con_ugm_mean$Proportion=Con_ugm_mean$mean/sum(as.numeric(as.character(statdf(Con_ugm,n = 6)[-1,1])),na.rm = TRUE)
  Con_ugm_mean$Proportion=round(Con_ugm_mean$Proportion,4)
  Con_ugm_mean=Con_ugm_mean[with(Con_ugm_mean, order(-mean)), ]

  #Con_ppbv_mean
  Con_ppbv_mean=data.frame(species=row.names(statdf(Con_ppbv)[-1,]),mean=as.numeric(as.character(statdf(Con_ppbv,n = 6)[-1,1])))
  Con_ppbv_mean$Proportion=Con_ppbv_mean$mean/sum(as.numeric(as.character(statdf(Con_ppbv,n = 6)[-1,1])),na.rm = TRUE)
  Con_ppbv_mean$Proportion=round(Con_ppbv_mean$Proportion,4)
  Con_ppbv_mean=Con_ppbv_mean[with(Con_ppbv_mean, order(-mean)), ]

  #loh_df_mean
  loh_df_mean=data.frame(species=row.names(statdf(loh_df)[-1,]),mean=as.numeric(as.character(statdf(loh_df,n = 6)[-1,1])))
  loh_df_mean$Proportion=loh_df_mean$mean/sum(as.numeric(as.character(statdf(loh_df,n = 6)[-1,1])),na.rm = TRUE)
  loh_df_mean$Proportion=round(loh_df_mean$Proportion,4)
  loh_df_mean=loh_df_mean[with(loh_df_mean, order(-mean)), ]

  #Con_ugm_group_mean
  Con_ugm_group_mean=data.frame(species=row.names(statdf(Con_ugm_group)[-1,]),mean=as.numeric(as.character(statdf(Con_ugm_group,n = 6)[-1,1])))
  Con_ugm_group_mean$Proportion=Con_ugm_group_mean$mean/sum(as.numeric(as.character(statdf(Con_ugm_group,n = 6)[-1,1])),na.rm = TRUE)
  Con_ugm_group_mean$Proportion=round(Con_ugm_group_mean$Proportion,4)
  Con_ugm_group_mean=Con_ugm_group_mean[with(Con_ugm_group_mean, order(-mean)), ]

  #Con_ppbv_group_mean
  Con_ppbv_group_mean=data.frame(species=row.names(statdf(Con_ppbv_group)[-1,]),mean=as.numeric(as.character(statdf(Con_ppbv_group,n = 6)[-1,1])))
  Con_ppbv_group_mean$Proportion=Con_ppbv_group_mean$mean/sum(as.numeric(as.character(statdf(Con_ppbv_group,n = 6)[-1,1])),na.rm = TRUE)
  Con_ppbv_group_mean$Proportion=round(Con_ppbv_group_mean$Proportion,4)
  Con_ppbv_group_mean=Con_ppbv_group_mean[with(Con_ppbv_group_mean, order(-mean)), ]

  #loh_df_group_mean
  loh_df_group_mean=data.frame(species=row.names(statdf(loh_df_group)[-1,]),mean=as.numeric(as.character(statdf(loh_df_group,n = 6)[-1,1])))
  loh_df_group_mean$Proportion=loh_df_group_mean$mean/sum(as.numeric(as.character(statdf(loh_df_group,n = 6)[-1,1])),na.rm = TRUE)
  loh_df_group_mean$Proportion=round(loh_df_group_mean$Proportion,4)
  loh_df_group_mean=loh_df_group_mean[with(loh_df_group_mean, order(-mean)), ]

  #suppress warnings temporarily?
  if(wamg==FALSE){options(warn=0)}

  #results
  results <- list(
	Con_ugm = Con_ugm,
	Con_ugm_mean = Con_ugm_mean,
	Con_ugm_group = Con_ugm_group,
	Con_ugm_group_mean = Con_ugm_group_mean,
	Con_ppbv = Con_ppbv,
	Con_ppbv_mean = Con_ppbv_mean,
	Con_ppbv_group = Con_ppbv_group,
	Con_ppbv_group_mean = Con_ppbv_group_mean,
	KOH_Result = name_df,
	LOH_Result = loh_df,
	LOH_Result_mean = loh_df_mean,
	LOH_Result_group = loh_df_group,
	LOH_Result_group_mean = loh_df_group_mean
  )
  return(results)
}
