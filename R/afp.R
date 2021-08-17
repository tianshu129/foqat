#' Calculate aerosol formation potential
#'
#' Calculate Aerosol Formation Potential (AFP) of VOC time series. Unit of AFP is ug/m3.
#' Note: for Chinese VOC name, please also use English punctuation.
#'
#' The CAS number is matched for each VOC speices (from column name), and the
#' average SOA yield (SOAY) is matched through the CAS number and used for time series calculation. \cr
#' The average SOAY comes from "W. Wu, B. Zhao, S. Wang, J. Hao, Ozone 
#' and secondary organic aerosol formation potential from anthropogenic volatile 
#' organic compounds emissions in China. J Environ Sci. 53, 224–237 (2017)".
#' Note: If input VOC species contain M,P-xylene, it will be automatically divided into m-xylene and P-xylene evenly.
#'
#' @param df dataframe contains time series.
#' @param inunit input's unit for VOC concentration. A character vector from these options: "ugm" or "ppbv". 
#' "ugm" means ug/m3. "ppbv" means part per billion volumn. The default vaule is "ppbv".
#' @param t Temperature, in Degrees Celsius, used to convert data in 
#' micrograms per cubic meter to standard conditions 
#' (25 Degrees Celsius, 101.325 kPa). By default, t equals to 25 Degrees Celsius.
#' @param p Pressure, in kPa, used to convert data in micrograms per cubic meter 
#' to standard conditions (25 Degrees Celsius, 101.325 kPa). 
#' By default, p equals to 101.325 kPa.
#' @param stcd logical. Does it output results in standard conditions? The default vaule is FALSE.
#' @param sortd logical value. It determines whether the VOC species
#' are sorted or not. By default, sortd has value "TRUE".
#' If TRUE, VOC species in time series will be arranged according to VOC group,
#'  relative molecular weight, and SOAY value.
#' @param chn logical. Dose colnames present as Chinese? The default vaule is FALSE.
#' @return  a list contains 5 tables:
#' SOAY_Result: matched SOAY value result;
#' AFP_Result: AFP time series of VOC by species;
#' AFP_Result_mean: the average value and proportion of AFP of VOC by species (sorted from large to small);
#' AFP_Result_group: AFP time series of VOC classified by groups;
#' AFP_Result_group_mean: the average value and proportion of AFP of VOC according to major groups (sorted from large to small).
#'
#' @export
#' @import magrittr
#' @importFrom stringr str_split_fixed

afp <- function(df, inunit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, chn=FALSE){

  #In case df is not a dataframe.
  temp_col_name <- colnames(df)
  df <- data.frame(df,stringsAsFactors = FALSE)
  colnames(df) <- temp_col_name

  if(chn==FALSE){  
	  #get VOC name by colnames of df
	  #if read from xlsx, replace "X" and "."
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  chemicalnames = gsub("\\.", "-", chemicalnames)
	  #if i-
	  chemicalnames = gsub("\\i-", "iso-", chemicalnames)

	  #build name_df
	  name_df = data.frame(name = chemicalnames,CAS = NA, Matched_Name = NA, SOAY = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

	  #search VOC name to get CAS Number from different sources, add cas, sources, mathed_name to name_df
	  ##firstly by NIST
	  for( i in 1:nrow(name_df)){
		tarname <- name_df[i,1]
		#test if name can be matched by names
		tarname=gsub("[^[:alnum:]]", "",tarname)
		tarname=tolower(tarname)
		tarid=eval(parse(text=paste0("grep('(?<![^;])",tarname,"(?![^;])',datacas$otn, value = FALSE, perl=TRUE)")))
		#if no, test if name can be matched by names
		if(length(tarid)!=1){
			tarname <- name_df[i,1]
			tarid=eval(parse(text=paste0("grep('(?<![^;])",tarname,"(?![^;])',datacas$CAS, value = FALSE, perl=TRUE)")))
		}
		#if finally get tarid (match)
		if(length(tarid)==1){
			tarid=as.numeric(tarid)
			name_df$CAS[i] = datacas$CAS[tarid]
			name_df$Matched_Name[i] = datacas$Description[tarid]
			name_df$SOAY[i] = datacas$soay[tarid]
			name_df$MW[i] = datacas$MWt[tarid]
			name_df$Group[i] = datacas$Group[tarid]
		}
	  }
	}else{
	  #build name_df
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  name_df = data.frame(name = chemicalnames,CAS = NA, Matched_Name = NA, SOAY = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)
	  #match table by chinese name

	  chn_name_db<-data.frame(str_split_fixed(gsub("\\/|\\,|\\-| ", "", datacas$chn), ';', 3))#change according to max chinese name vector
	  for(k in 1:nrow(name_df)){
		chn_df<-data.frame(str_split_fixed(gsub("\\,|\\-| ", "", datacas$chn), ';', 2))
		x=which(chn_df == gsub("\\,|\\,|\\-| ", "", name_df$name[k]), arr.ind = TRUE)[1]
		df_null=data.frame(datacas[x,])
		if(nrow(df_null)!=0){
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]		  
		  name_df$SOAY[as.numeric(k)] = df_null$soay[1]
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
	  name_df = name_df[with(name_df, order(Group, MW, SOAY)), ]
	  df[,2:ncol(df)]=df[,name_df$raw_order+1]
	  colnames(df)[2:ncol(df)]=colnames(df)[name_df$raw_order+1]
  }

  #set concentration df, multiple df with SOAY in name_df
  afp_df = df
  r = 22.4*(273.15+t)*101.325/(273.15*p)
  r2 = (298.15*p)/((273.15+t)*101.325)
  if(inunit=="ugm"){
  	Con_ppbv = df
	Con_ugm = df
	if(stcd==TRUE){
		Con_ugm[,2:ncol(df)] = Con_ugm[,2:ncol(df)]/r2	
	}
	afp_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) Con_ugm[,x] * as.numeric(name_df$SOAY)[x-1]),ncol = ncol(df)-1))	
	Con_ppbv[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(r/name_df$MW)[x-1]),ncol = ncol(df)-1))
  }else if(inunit=="ppbv"){
    Con_ppbv = df
	Con_ugm = df
	if(stcd==FALSE){
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/r)[x-1]),ncol = ncol(df)-1))		
	}else{
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/24.45016)[x-1]),ncol = ncol(df)-1))		
	}
	afp_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) Con_ugm[,x] * as.numeric(name_df$SOAY)[x-1]),ncol = ncol(df)-1))
  }else{
    print("input-unit error")
  }

  #vector of group names
  gn_list = c("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown")

  #generate group df
  Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  afp_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)

  #sum up columns
  for(gn in 1:length(gn_list)){
	gn_sub_index = which(name_df$Group == gn_list[gn])
	if(length(gn_sub_index)!=0){
		if(length(gn_sub_index)==1){
			Con_ppbv_group[,gn+1]=Con_ppbv[,gn_sub_index+1]
			Con_ugm_group[,gn+1]=Con_ugm[,gn_sub_index+1]
			afp_df_group[,gn+1]=afp_df[,gn_sub_index+1]
		}else{
			Con_ppbv_group[,gn+1]=rowSums(Con_ppbv[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ppbv[,gn_sub_index+1]))
			Con_ugm_group[,gn+1]=rowSums(Con_ugm[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ugm[,gn_sub_index+1]))
			afp_df_group[,gn+1]=rowSums(afp_df[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(afp_df[,gn_sub_index+1]))
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

  #afp_df_mean
  afp_df_mean=data.frame(species=row.names(statdf(afp_df)[-1,]),mean=as.numeric(as.character(statdf(afp_df,n = 6)[-1,1])))
  afp_df_mean$Proportion=afp_df_mean$mean/sum(as.numeric(as.character(statdf(afp_df,n = 6)[-1,1])),na.rm = TRUE)
  afp_df_mean$Proportion=round(afp_df_mean$Proportion,4)
  afp_df_mean=afp_df_mean[with(afp_df_mean, order(-mean)), ]

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

  #afp_df_group_mean
  afp_df_group_mean=data.frame(species=row.names(statdf(afp_df_group)[-1,]),mean=as.numeric(as.character(statdf(afp_df_group,n = 6)[-1,1])))
  afp_df_group_mean$Proportion=afp_df_group_mean$mean/sum(as.numeric(as.character(statdf(afp_df_group,n = 6)[-1,1])),na.rm = TRUE)
  afp_df_group_mean$Proportion=round(afp_df_group_mean$Proportion,4)
  afp_df_group_mean=afp_df_group_mean[with(afp_df_group_mean, order(-mean)), ]

  #results
  results <- list(
	SOAY_Result = name_df,
	afp_Result = afp_df,
	afp_Result_mean = afp_df_mean,
	afp_Result_group = afp_df_group,
	afp_Result_group_mean = afp_df_group_mean
  )
  return(results)
}