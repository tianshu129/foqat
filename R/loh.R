#' Calculate OH reactivity
#'
#' Calculate OH reactivity of VOC time series in 25 degree celsius. 
#' Note: for Chinese VOC name, please also use English punctuation.
#'
#' The CAS number is matched for each VOC speices (from column name), and the
#' OH Rate Constant is matched through the CAS number and used for time series calculation. \cr
#' The OH Rate Constant comes from 'AopWin v1.92' in 25 degree celsius.
#' Note: If input VOC species contain M,P-xylene, it will be automatically divided into m-xylene and P-xylene evenly.
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
#' @param stcd logical. Does it output results of mass concentrations in standard conditions? The default vaule is FALSE.
#' @param sortd logical value. It determines whether the VOC species
#' are sorted or not. By default, sortd has value "TRUE".
#' If TRUE, VOC species in time series will be arranged according to VOC group,
#'  relative molecular weight, and OH Rate Constant.
#' @param stcd logical. Does it output the concentration in standard condition? 
#' The default vaule is FALSE.
#' @param colid column index for date-time. The default value is 1.
#' @param atk logical. use kOH value from atk or not? If not, kOH comes from 'AopWin v1.92' will be used. The default vaule is TRUE.
#' @param chn logical. Dose colnames present as Chinese? The default vaule is FALSE.
#' @return  a list contains 5 tables:
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
#' @importFrom utils download.file
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @import magrittr
#' @importFrom stringr str_split_fixed


loh <- function(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, colid = 1, atk=TRUE, chn=FALSE){

    #generate datacasv2 according to datacas
  datacasv2=datacas
  if(atk==TRUE){
	datacasv2$koh=datacasv2$koh_atk
	datacasv2$koh_type=NA
	datacasv2$koh_type[which(!is.na(datacasv2$koh))]="Atkinson"
	datacasv2$koh_type[which(is.na(datacasv2$koh)&!is.na(datacasv2$koh_aop))]="AopWin"
	datacasv2$koh[which(is.na(datacasv2$koh)&!is.na(datacasv2$koh_aop))]=datacasv2$koh_aop[which(is.na(datacasv2$koh)&!is.na(datacasv2$koh_aop))]
  }else{
	datacasv2$koh=datacasv2$koh_aop
	datacasv2$koh_type[which(!is.na(datacasv2$koh))]="AopWin"
  }

  #set colid
  if(colid != 1){
    df[,c(1,colid)] = df[,c(colid,1)]
    colnames(df)[c(1,colid)] = colnames(df)[c(colid,1)]
  }

  #In case df is not a dataframe.
  temp_col_name <- colnames(df)
  df <- data.frame(df,stringsAsFactors = FALSE)
  colnames(df) <- temp_col_name
  
  if(chn==FALSE){
	  #In case df includes m,p-Xylene
	  colnames_short = gsub("\\,|\\-| ", "", tolower(colnames(df)))
	  if("mpxylene" %in% colnames_short){
		  xyleneid=which(colnames_short %in% "mpxylene")
		  df=df[,c(1:xyleneid,xyleneid:ncol(df))]
		  df[,c(xyleneid,(xyleneid+1))]=df[,c(xyleneid,(xyleneid+1))]/2
		  colnames(df)[c(xyleneid,(xyleneid+1))]=c("m-Xylene","p-Xylene")
	  }

	  #get VOC name by colnames of df
	  #if read from xlsx, replace "X" and "."
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  chemicalnames = gsub("\\.", "-", chemicalnames)
	  #if i-
	  chemicalnames = gsub("\\i-", "iso-", chemicalnames)

	  #build name_df
	  name_df = data.frame(name = chemicalnames,CAS = NA, Source = NA, Matched_Name = NA, koh = NA, koh_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

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
	  file.remove("scrapedpage.html")

	  #match koh by different sources
	  ##get CAS from NIST, match name by CAS
	  a=lapply(name_df$CAS[which(name_df$Source=="NIST"&!is.na(name_df$CAS))], function(i) grep(i, datacasv2$CAS))
	  a=unlist(lapply(a,function(x) if(identical(x,integer(0))) ' ' else x))
	  name_df$koh[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacasv2$koh[as.numeric(a)]
	  name_df$koh_type[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacasv2$koh_type[as.numeric(a)]
	  name_df$Matched_Name[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacasv2$Description[as.numeric(a)]
	  name_df$MW[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacasv2$MWt[as.numeric(a)]
	  name_df$Group[which(name_df$Source=="NIST"&!is.na(name_df$CAS))] = datacasv2$Group[as.numeric(a)]

	  #if it is matched by CAS in NIST and matched by name in Carter paper, but it doesn't have CAS in Carter paper.
	  for(k in which(!is.na(name_df$Source)&is.na(name_df$koh))){
		tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
		tar=gsub(" ", "", tolower(name_df$name[k]), fixed = TRUE)
		df_null=data.frame(datacasv2[tarlist %in% tar,])
		if(nrow(df_null)!=0){
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  #name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$koh[as.numeric(k)] = df_null$koh[1]
		  name_df$koh_type[as.numeric(k)] = df_null$koh_type[1]
		  name_df$Source[as.numeric(k)] = "CAS is found in NIST. But it only has name in Carter paper 2010"
		  name_df$MW[as.numeric(k)] = df_null$MWt[1]
		  name_df$Group[as.numeric(k)] = df_null$Group[1]
		}
	  }

	  #if it isn't found in NIST, but its name is matched by Carter paper.
	  for(k in which(is.na(name_df$Source))){
		tarlist=gsub(" ", "", tolower(datacas$Description), fixed = TRUE)
		tar=gsub(" ", "", tolower(name_df$name[k]), fixed = TRUE)
		df_null=data.frame(datacasv2[tarlist %in% tar,])
		if(nrow(df_null)!=0){
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$koh[as.numeric(k)] = df_null$koh[1]
		  name_df$koh_type[as.numeric(k)] = df_null$koh_type[1]
		  name_df$Source[as.numeric(k)] = "Carter paper 2010"
		  name_df$MW[as.numeric(k)] = df_null$MWt[1]
		  name_df$Group[as.numeric(k)] = df_null$Group[1]
		}
	  }
	}else{
	  #build name_df
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  name_df = data.frame(name = chemicalnames,CAS = NA, Source = NA, Matched_Name = NA, koh = NA, koh_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

	  #match table by chinese name
	  chn_name_db<-data.frame(str_split_fixed(gsub("\\/|\\,|\\-| ", "", datacasv2$chn), ';', 3))#change according to max chinese name vector
	  for(k in 1:nrow(name_df)){
		chn_df<-data.frame(str_split_fixed(gsub("\\,|\\-| ", "", datacasv2$chn), ';', 2))
		x=which(chn_df == gsub("\\,|\\,|\\-| ", "", name_df$name[k]), arr.ind = TRUE)[1]
		df_null=data.frame(datacasv2[x,])
		if(nrow(df_null)!=0){
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$koh[as.numeric(k)] = df_null$koh[1]
		  name_df$koh_type[as.numeric(k)] = df_null$koh_type[1]
		  name_df$Source[as.numeric(k)] = "Chinese"
		  name_df$MW[as.numeric(k)] = df_null$MWt[1]
		  name_df$Group[as.numeric(k)] = df_null$Group[1]
		}
	  }
	}
	


  #set GROUP to Unknown for NA group
  name_df$Group[is.na(name_df$Group)] = "Unknown"

  #set GROUP to BVOC for BVOC group
  name_df$Group[name_df$CAS %in% c('80-56-8','127-91-3','78-79-5','138-86-3')] = "BVOC"

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
  r2 = (298.15*p)/((273.15+t)*101.325)
  Avogadro = 6.022e23
  if(unit=="ugm"){
	Con_ppbv = df
	Con_ugm = df
	if(stcd==TRUE){
		Con_ugm[,2:ncol(df)] = Con_ugm[,2:ncol(df)]/r2	
	}	
	Con_ppbv[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(r/name_df$MW)[x-1]),ncol = ncol(df)-1))
	loh_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$koh*Avogadro*1e-12/(name_df$MW*r2))[x-1]),ncol = ncol(df)-1))
  }else if(unit=="ppbv"){
	Con_ppbv = df
	Con_ugm = df
	if(stcd==FALSE){
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/r)[x-1]),ncol = ncol(df)-1))		
	}else{
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/24.45016)[x-1]),ncol = ncol(df)-1))	
	}
	loh_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x] * 
		as.numeric(name_df$koh*Avogadro*1e-12/24.45016)[x-1]),ncol = ncol(df)-1))
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

  #results
  results <- list(
	KOH_Result = name_df,
	LOH_Result = loh_df,
	LOH_Result_mean = loh_df_mean,
	LOH_Result_group = loh_df_group,
	LOH_Result_group_mean = loh_df_group_mean
  )
  return(results)
}
