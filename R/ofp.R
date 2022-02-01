#' Calculate ozone formation potential
#'
#' Calculate Ozone Formation Potential (OFP) of VOC time series.
#' Note: for Chinese VOC name, please also use English punctuation.
#'
#' The CAS number is matched for each VOC speices (from column name), and the
#' Maximum Incremental Reactivity (MIR) value is matched through the CAS number and used for time series calculation. \cr
#' The MIR value comes from <https://ww2.arb.ca.gov/sites/default/files/classic/regact/2009/mir2009/mir10.pdf>, 
#' Zhang et al.(2021) <doi:10.5194/acp-21-11053-2021>.
#'
#' @param df dataframe contains time series.
#' @param inunit input's unit for VOC concentration. A character vector from these options: "ugm" or "ppbv". 
#' "ugm" means ug/m3. "ppbv" means part per billion volumn. The default vaule is "ppbv".
#' @param outunit output's unit for VOC concentration. A character from these options: "ugm" or "ppbv". 
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
#'  relative molecular weight, and MIR value.
#' @param chn logical. Dose colnames present as Chinese? The default vaule is FALSE.
#' @param mtype text. "usa" for MIR value from USA, "chn" for MIR value from CHINA.
#' @param bvoc logical. Whether you want to list BVOC as a separate VOC group? The default vaule is TRUE.
#' @return  a list contains 5 tables:
#' MIR_Result: matched MIR value result;
#' OFP_Result: OFP time series of VOC by species;
#' OFP_Result_mean: the average value and proportion of OFP of VOC by species (sorted from large to small);
#' OFP_Result_group: OFP time series of VOC classified by groups;
#' OFP_Result_group_mean: the average value and proportion of OFP of VOC according to major groups (sorted from large to small).
#'
#' @export
#' @examples
#' ofp(voc)
#' @import magrittr
#' @importFrom stringr str_split_fixed

ofp <- function(df, inunit = "ppbv", outunit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, chn=FALSE, mtype="usa", bvoc=TRUE){

  #generate datacasv2 according to datacas
  datacasv2=datacas
  if(mtype=="usa"){
	datacasv2$MIR=datacasv2$New
	datacasv2$MIR_type="USA"
  }else{
	datacasv2$MIR=datacasv2$mir_cn
	datacasv2$MIR_type="CHINA"
  }

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
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Matched_Name = NA, MIR = NA, MIR_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

	  #search VOC name to get CAS Number from different sources, add cas, sources, mathed_name to name_df
	  ##firstly by NIST
	  for( i in 1:nrow(name_df)){
		tarname <- name_df[i,1]
		#test if name can be matched by names
		tarname=gsub("[^[:alnum:]]", "",tarname)
		tarname=tolower(tarname)
		tarid=eval(parse(text=paste0("grep('(?<![^;])",tarname,"(?![^;])',datacasv2$otn, value = FALSE, perl=TRUE)")))
		#if no, test if name can be matched by names
		if(length(tarid)!=1){
			tarname <- name_df[i,1]
			tarid=eval(parse(text=paste0("grep('(?<![^;])",tarname,"(?![^;])',datacasv2$CAS, value = FALSE, perl=TRUE)")))
		}
		#if finally get tarid (match)
		if(length(tarid)==1){
			tarid=as.numeric(tarid)
			name_df$CAS[i] = datacasv2$CAS[tarid]
			name_df$Matched_Name[i] = datacasv2$Description[tarid]
			name_df$MIR[i] = datacasv2$MIR[tarid]
			name_df$MIR_type[i] = datacasv2$MIR_type[tarid]
			name_df$MW[i] = datacasv2$MWt[tarid]
			name_df$Group[i] = datacasv2$Group[tarid]
		}
	  }
	}else{
	  #build name_df
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Matched_Name = NA, MIR = NA, MIR_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)
	  #match table by chinese name

	  chn_name_db<-data.frame(str_split_fixed(gsub("\\/|\\,|\\-| ", "", datacasv2$chn), ';', 3))#change according to max chinese name vector
	  for(k in 1:nrow(name_df)){
		chn_df<-data.frame(str_split_fixed(gsub("\\,|\\-| ", "", datacasv2$chn), ';', 2))
		x=which(chn_df == gsub("\\,|\\,|\\-| ", "", name_df$Name[k]), arr.ind = TRUE)[1]
		df_null=data.frame(datacasv2[x,])
		if(nrow(df_null)!=0){
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  name_df$MIR[as.numeric(k)] = df_null$MIR[1]
		  name_df$MIR_type[as.numeric(k)] = df_null$MIR_type[1]
		  name_df$MW[as.numeric(k)] = df_null$MWt[1]
		  name_df$Group[as.numeric(k)] = df_null$Group[1]
		}
	  }
	}  

  #set GROUP to Unknown for NA group
  name_df$Group[is.na(name_df$Group)] = "Unknown"

  #set GROUP to BVOC for BVOC group
  if(bvoc==TRUE){
	name_df$Group[name_df$CAS %in% c('80-56-8','127-91-3','78-79-5','138-86-3')] = "BVOC"
  }
  
  #raw_order
  name_df$Raw_order = seq.int(nrow(name_df))

  #set order for voc species in df and name_df
  if(sortd==TRUE){
	  #order by 2 columns
	  if(bvoc==TRUE){
		name_df$Group <- factor(name_df$Group, levels = c("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown"))
	  }else{
		name_df$Group <- factor(name_df$Group, levels = c("Alkanes", "Alkenes", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown"))
	  }
	  name_df = name_df[with(name_df, order(Group, MW, MIR)), ]
	  df[,2:ncol(df)]=df[,name_df$Raw_order+1]
	  colnames(df)[2:ncol(df)]=colnames(df)[name_df$Raw_order+1]
  }

  #set concentration df, multiple df with MIR in name_df
  ofp_df = df
  r = 22.4*(273.15+t)*101.325/(273.15*p)
  r2 = (298.15*p)/((273.15+t)*101.325)
  if(inunit=="ugm"){
  	Con_ppbv = df
	Con_ugm = df
	if(stcd==TRUE){
		Con_ugm[,2:ncol(df)] = Con_ugm[,2:ncol(df)]/r2	
	}
	ofp_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) Con_ugm[,x] * as.numeric(name_df$MIR)[x-1]),ncol = ncol(df)-1))	
	Con_ppbv[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(r/name_df$MW)[x-1]),ncol = ncol(df)-1))
  }else if(inunit=="ppbv"){
    Con_ppbv = df
	Con_ugm = df
	if(stcd==FALSE){
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/r)[x-1]),ncol = ncol(df)-1))		
	}else{
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/24.45016)[x-1]),ncol = ncol(df)-1))		
	}
	ofp_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) Con_ugm[,x] * as.numeric(name_df$MIR)[x-1]),ncol = ncol(df)-1))
  }else{
    print("input-unit error")
  }

  #vector of group names
  if(bvoc==TRUE){
	gn_list = c("Alkanes", "Alkenes", "BVOC", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown")
  }else{
	gn_list = c("Alkanes", "Alkenes", "Alkynes", "Aromatic_Hydrocarbons", "Oxygenated_Organics", "Other_Organic_Compounds", "Unknown")
  }

  #generate group df
  if(bvoc==TRUE){
	Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
	Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
	ofp_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  }else{
	Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
	Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA) 
	ofp_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA) 
  }

  #sum up columns
  for(gn in 1:length(gn_list)){
	gn_sub_index = which(name_df$Group == gn_list[gn])
	if(length(gn_sub_index)!=0){
		if(length(gn_sub_index)==1){
			Con_ppbv_group[,gn+1]=Con_ppbv[,gn_sub_index+1]
			Con_ugm_group[,gn+1]=Con_ugm[,gn_sub_index+1]
			ofp_df_group[,gn+1]=ofp_df[,gn_sub_index+1]
		}else{
			Con_ppbv_group[,gn+1]=rowSums(Con_ppbv[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ppbv[,gn_sub_index+1]))
			Con_ugm_group[,gn+1]=rowSums(Con_ugm[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ugm[,gn_sub_index+1]))
			ofp_df_group[,gn+1]=rowSums(ofp_df[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(ofp_df[,gn_sub_index+1]))
		}
	}
  }

  #remove NA columns
  Con_ppbv_group <- Con_ppbv_group[,colSums(is.na(Con_ppbv_group))<nrow(Con_ppbv_group)]
  Con_ugm_group <- Con_ugm_group[,colSums(is.na(Con_ugm_group))<nrow(Con_ugm_group)]
  ofp_df_group <- ofp_df_group[,colSums(is.na(ofp_df_group))<nrow(ofp_df_group)]
  
  #Con_ugm_stat
  Con_ugm_stat=data.frame(Species=row.names(statdf(Con_ugm)),Mean=as.numeric(as.character(statdf(Con_ugm,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ugm,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ugm,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ugm,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ugm,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ugm,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ugm,n = 3)[,6])))
  Con_ugm_stat$Proportion=Con_ugm_stat$Mean/sum(as.numeric(as.character(statdf(Con_ugm,n = 3)[,1])),na.rm = TRUE)
  Con_ugm_stat$Proportion=round(Con_ugm_stat$Proportion,4)
  Con_ugm_stat=Con_ugm_stat[with(Con_ugm_stat, order(-Mean)), ]

  #Con_ppbv_stat
  Con_ppbv_stat=data.frame(Species=row.names(statdf(Con_ppbv)),Mean=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,6])))
  Con_ppbv_stat$Proportion=Con_ppbv_stat$Mean/sum(as.numeric(as.character(statdf(Con_ppbv,n = 3)[,1])),na.rm = TRUE)
  Con_ppbv_stat$Proportion=round(Con_ppbv_stat$Proportion,4)
  Con_ppbv_stat=Con_ppbv_stat[with(Con_ppbv_stat, order(-Mean)), ]

  #ofp_df_stat
  ofp_df_stat=data.frame(Species=row.names(statdf(ofp_df)),Mean=as.numeric(as.character(statdf(ofp_df,n = 3)[,1])),SD=as.numeric(as.character(statdf(ofp_df,n = 3)[,2])),Min=as.numeric(as.character(statdf(ofp_df,n = 3)[,3])),Q25=as.numeric(as.character(statdf(ofp_df,n = 3)[,4])),Q50=as.numeric(as.character(statdf(ofp_df,n = 3)[,5])),Q75=as.numeric(as.character(statdf(ofp_df,n = 3)[,6])),Max=as.numeric(as.character(statdf(ofp_df,n = 3)[,6])))
  ofp_df_stat$Proportion=ofp_df_stat$Mean/sum(as.numeric(as.character(statdf(ofp_df,n = 3)[,1])),na.rm = TRUE)
  ofp_df_stat$Proportion=round(ofp_df_stat$Proportion,4)
  ofp_df_stat=ofp_df_stat[with(ofp_df_stat, order(-Mean)), ]

  #Con_ugm_group_stat
  Con_ugm_group_stat=data.frame(Species=row.names(statdf(Con_ugm_group)),Mean=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])))
  Con_ugm_group_stat$Proportion=Con_ugm_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,1])),na.rm = TRUE)
  Con_ugm_group_stat$Proportion=round(Con_ugm_group_stat$Proportion,4)
  Con_ugm_group_stat=Con_ugm_group_stat[with(Con_ugm_group_stat, order(-Mean)), ]

  #Con_ppbv_group_stat
  Con_ppbv_group_stat=data.frame(Species=row.names(statdf(Con_ppbv_group)),Mean=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,2])),min=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])))
  Con_ppbv_group_stat$Proportion=Con_ppbv_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),na.rm = TRUE)
  Con_ppbv_group_stat$Proportion=round(Con_ppbv_group_stat$Proportion,4)
  Con_ppbv_group_stat=Con_ppbv_group_stat[with(Con_ppbv_group_stat, order(-Mean)), ]

  #ofp_df_group_stat
  ofp_df_group_stat=data.frame(Species=row.names(statdf(ofp_df_group)),Mean=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(ofp_df_group,n = 3)[,6])))
  ofp_df_group_stat$Proportion=ofp_df_group_stat$Mean/sum(as.numeric(as.character(statdf(ofp_df_group,n = 3)[,1])),na.rm = TRUE)
  ofp_df_group_stat$Proportion=round(ofp_df_group_stat$Proportion,4)
  ofp_df_group_stat=ofp_df_group_stat[with(ofp_df_group_stat, order(-Mean)), ]

  if(outunit == "ppbv"){
	ofp_df[,-1]=round(ofp_df[,-1]/48*24.45, 3)
	ofp_df_stat[,2:8]=round(ofp_df_stat[,2:8]/48*24.45, 3)
	ofp_df_group[,-1]=round(ofp_df_group[,-1]/48*24.45, 3)
	ofp_df_group_stat[,2:8]=round(ofp_df_group_stat[,2:8]/48*24.45, 3)
  }

  #results
  results <- list(
	MIR_Result = name_df,
	OFP_Result = ofp_df,
	OFP_Result_stat = ofp_df_stat,
	OFP_Result_group = ofp_df_group,
	OFP_Result_group_stat = ofp_df_group_stat
  )
  return(results)
}