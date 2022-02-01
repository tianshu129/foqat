#' Convertion and analysis of VOC concentrations
#'
#' convert unit of VOCs between micrograms per cubic meter (ugm) and parts
#' per billion by volume (ppbv); conduct statistics of VOC concentrations.
#' Note: for Chinese VOC name, please also use English punctuation.
#'
#' The CAS number was matched for each VOC speices (from column name), and the
#' Molecular Weight (MW) value and Maximum Incremental Reactivity (MIR) value are matched through the CAS number and used for time series calculation. \cr
#' The MIR value comes from "Carter, W. P. (2009). Updated maximum incremental
#' reactivity scale and hydrocarbon bin reactivities for regulatory applications.
#' California Air Resources Board Contract, 2009, 339" (revised January 28, 2010).
#'
#' @param df dataframe contains time series.
#' @param unit unit for VOC concentration. A character vector from these options: "ugm" or "ppbv". "ugm" means ug/m3. "ppbv" means part per billion volumn.
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
#' @param bvoc logical. Whether you want to list BVOC as a separate VOC group? The default vaule is TRUE.
#' @return  a list contains 9 tables:
#' MW_Result: matched Molecular Weight (MW) value result;
#' Con_ugm: time series of VOC mass concentration by species;
#' Con_ugm_mean: the average mass concentration and proportion of VOC by species (sorted from large to small);
#' Con_ugm_group: time series of VOC mass concentration classified by groups;
#' Con_ugm_group_mean: the average value and proportion of VOC mass concentration (sorted from large to small) according to major groups;
#' Con_ppbv: time series of VOC volume concentration by species;
#' Con_ppbv_mean: the average volume concentration and proportion of VOC by species (sorted from large to small);
#' Con_ppbv_group: time series of VOC volume concentration according to major groups;
#' Con_ppbv_group_mean: VOC volume concentration average and proportion (sorted from large to small) according to major groups;
#'
#' @export
#' @examples
#' vocct(voc)
#' @import magrittr
#' @importFrom stringr str_split_fixed

vocct <- function(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, chn=FALSE, bvoc=TRUE){

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
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Matched_Name = NA, MIR = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

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
			name_df$MIR[i] = datacas$New[tarid]
			name_df$MW[i] = datacas$MWt[tarid]
			name_df$Group[i] = datacas$Group[tarid]			
		}
	  }	
	}else{
	  #build name_df
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Source = NA, Matched_Name = NA, MIR = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

	  #match table by chinese name
	  chn_name_db<-data.frame(str_split_fixed(gsub("\\/|\\,|\\-| ", "", datacas$chn), ';', 3))#change according to max chinese name vector
	  for(k in 1:nrow(name_df)){
		chn_df<-data.frame(str_split_fixed(gsub("\\,|\\-| ", "", datacas$chn), ';', 2))
		x=which(chn_df == gsub("\\,|\\,|\\-| ", "", name_df$Name[k]), arr.ind = TRUE)[1]
		df_null=data.frame(datacas[x,])
		if(nrow(df_null)!=0){
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$MIR[as.numeric(k)] = df_null$New[1]
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
  r = 22.4*(273.15+t)*101.325/(273.15*p)
  r2 = (298.15*p)/((273.15+t)*101.325)
  if(unit=="ugm"){
  	Con_ppbv = df
	Con_ugm = df
	if(stcd==TRUE){
		Con_ugm[,2:ncol(df)] = Con_ugm[,2:ncol(df)]/r2	
	}
	Con_ppbv[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(r/name_df$MW)[x-1]),ncol = ncol(df)-1))
  }else if(unit=="ppbv"){
    Con_ppbv = df
	Con_ugm = df
	if(stcd==FALSE){
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/r)[x-1]),ncol = ncol(df)-1))		
	}else{
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/24.45016)[x-1]),ncol = ncol(df)-1))		
	}
  }else{
    print("unit error")
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
  }else{
	Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
	Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA) 
  }
  
  #sum up columns
  for(gn in 1:length(gn_list)){
	gn_sub_index = which(name_df$Group == gn_list[gn])
	if(length(gn_sub_index)!=0){
		if(length(gn_sub_index)==1){
			Con_ppbv_group[,gn+1]=Con_ppbv[,gn_sub_index+1]
			Con_ugm_group[,gn+1]=Con_ugm[,gn_sub_index+1]
		}else{
			Con_ppbv_group[,gn+1]=rowSums(Con_ppbv[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ppbv[,gn_sub_index+1]))
			Con_ugm_group[,gn+1]=rowSums(Con_ugm[,gn_sub_index+1], na.rm=TRUE) *NA^!rowSums(!is.na(Con_ugm[,gn_sub_index+1]))
		}
	}
  }
  
  #remove NA columns
  Con_ppbv_group <- Con_ppbv_group[,colSums(is.na(Con_ppbv_group))<nrow(Con_ppbv_group)]
  Con_ugm_group <- Con_ugm_group[,colSums(is.na(Con_ugm_group))<nrow(Con_ugm_group)]
  
  #Con_ugm_stat
  Con_ugm_stat=data.frame(Species=row.names(statdf(Con_ugm)),Mean=as.numeric(as.character(statdf(Con_ugm,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ugm,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ugm,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ugm,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ugm,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ugm,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ugm,n = 3)[,6])))
  Con_ugm_stat$Proportion=Con_ugm_stat$Mean/sum(as.numeric(as.character(statdf(Con_ugm,n = 3)[,1])),na.rm = TRUE)
  Con_ugm_stat$Proportion=round(Con_ugm_stat$Proportion,4)
  Con_ugm_stat=Con_ugm_stat[with(Con_ugm_stat, order(-Mean)), ]

  #Con_ppbv_stat
  Con_ppbv_stat=data.frame(species=row.names(statdf(Con_ppbv)),Mean=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,1])),sd=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ppbv,n = 3)[,6])))
  Con_ppbv_stat$Proportion=Con_ppbv_stat$Mean/sum(as.numeric(as.character(statdf(Con_ppbv,n = 6)[,1])),na.rm = TRUE)
  Con_ppbv_stat$Proportion=round(Con_ppbv_stat$Proportion,4)
  Con_ppbv_stat=Con_ppbv_stat[with(Con_ppbv_stat, order(-Mean)), ]

  #Con_ugm_group_stat
  Con_ugm_group_stat=data.frame(Species=row.names(statdf(Con_ugm_group)),Mean=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,2])),min=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])))
  Con_ugm_group_stat$Proportion=Con_ugm_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ugm_group,n = 6)[,1])),na.rm = TRUE)
  Con_ugm_group_stat$Proportion=round(Con_ugm_group_stat$Proportion,4)
  Con_ugm_group_stat=Con_ugm_group_stat[with(Con_ugm_group_stat, order(-Mean)), ]

  #Con_ppbv_group_stat
  Con_ppbv_group_stat=data.frame(Species=row.names(statdf(Con_ppbv_group)),Mean=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])))
  Con_ppbv_group_stat$Proportion=Con_ppbv_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),na.rm = TRUE)
  Con_ppbv_group_stat$Proportion=round(Con_ppbv_group_stat$Proportion,4)
  Con_ppbv_group_stat=Con_ppbv_group_stat[with(Con_ppbv_group_stat, order(-Mean)), ]

  #results
  results <- list(
	MW_Result = name_df,
	Con_ugm = Con_ugm,
	Con_ugm_stat = Con_ugm_stat,
	Con_ugm_group = Con_ugm_group,
	Con_ugm_group_stat = Con_ugm_group_stat,
	Con_ppbv = Con_ppbv,
	Con_ppbv_stat = Con_ppbv_stat,
	Con_ppbv_group = Con_ppbv_group,
	Con_ppbv_group_stat = Con_ppbv_group_stat
  )
  return(results)
}
