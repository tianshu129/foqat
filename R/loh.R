#' Calculate OH reactivity
#'
#' Calculate OH reactivity of VOC time series in 25 degree celsius. 
#' Note: for Chinese VOC name, please also use English punctuation.
#'
#' The CAS number is matched for each VOC speices (from column name), and the
#' OH Rate Constant is matched through the CAS number and used for time series calculation. \cr
#' The OH Rate Constant comes from 'AopWin v1.92' in 25 degree celsius.
#'
#' @param df dataframe contains time series.
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
#' @param atk logical. use kOH value from atk or not? If not, kOH comes from 'AopWin v1.92' will be used. The default vaule is TRUE.
#' @param chn logical. Dose colnames present as Chinese? The default vaule is FALSE.
#' @param bvoc logical. Whether you want to list BVOC as a separate VOC group? The default vaule is TRUE.
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

#' @import magrittr
#' @importFrom stringr str_split_fixed


loh <- function(df, unit = "ppbv", t = 25, p = 101.325, stcd=FALSE, sortd =TRUE, atk=TRUE, chn=FALSE, bvoc=TRUE){

    #generate datacasv2 according to datacas
  datacasv2=datacas
  if(atk==TRUE){
	datacasv2$Koh=datacasv2$koh_atk
	datacasv2$Koh_type=NA
	datacasv2$Koh_type[which(!is.na(datacasv2$Koh))]="Atkinson"
	datacasv2$Koh_type[which(is.na(datacasv2$Koh)&!is.na(datacasv2$koh_aop))]="AopWin"
	datacasv2$Koh[which(is.na(datacasv2$Koh)&!is.na(datacasv2$koh_aop))]=datacasv2$koh_aop[which(is.na(datacasv2$Koh)&!is.na(datacasv2$koh_aop))]
  }else{
	datacasv2$Koh=datacasv2$koh_aop
	datacasv2$koh_type[which(!is.na(datacasv2$Koh))]="AopWin"
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
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Matched_Name = NA, Koh = NA, Koh_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

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
			name_df$Koh[i] = datacasv2$Koh[tarid]
			name_df$Koh_type[i] = datacasv2$Koh_type[tarid]
			name_df$MW[i] = datacasv2$MWt[tarid]
			name_df$Group[i] = datacasv2$Group[tarid]
		}
	  }
	}else{
	  #build name_df
	  colnm_df = colnames(df)[2:ncol(df)]
	  chemicalnames = ifelse(substr(colnm_df, 1, 1) == "X", sub("^.", "", colnm_df), colnm_df)
	  name_df = data.frame(Name = chemicalnames,CAS = NA, Matched_Name = NA, Koh = NA, Koh_type = NA, MW = NA, Group = NA, stringsAsFactors = FALSE)

	  #match table by chinese name
	  chn_name_db<-data.frame(str_split_fixed(gsub("\\/|\\,|\\-| ", "", datacasv2$chn), ';', 3))#change according to max chinese name vector
	  for(k in 1:nrow(name_df)){
		chn_df<-data.frame(str_split_fixed(gsub("\\,|\\-| ", "", datacasv2$chn), ';', 2))
		x=which(chn_df == gsub("\\,|\\,|\\-| ", "", name_df$Name[k]), arr.ind = TRUE)[1]
		df_null=data.frame(datacasv2[x,])
		if(nrow(df_null)!=0){
		  name_df$CAS[as.numeric(k)] = df_null$CAS[1]
		  name_df$Matched_Name[as.numeric(k)] = df_null$Description[1]
		  name_df$Koh[as.numeric(k)] = df_null$Koh[1]
		  name_df$Koh_type[as.numeric(k)] = df_null$Koh_type[1]
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
	  name_df = name_df[with(name_df, order(Group, MW, Koh)), ]
	  df[,2:ncol(df)]=df[,name_df$Raw_order+1]
	  colnames(df)[2:ncol(df)]=colnames(df)[name_df$Raw_order+1]
  }

  #set concentration df, multiple df with Koh in name_df
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
	loh_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x] * as.numeric(name_df$Koh*Avogadro*1e-12/(name_df$MW*r2))[x-1]),ncol = ncol(df)-1))
  }else if(unit=="ppbv"){
	Con_ppbv = df
	Con_ugm = df
	if(stcd==FALSE){
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/r)[x-1]),ncol = ncol(df)-1))		
	}else{
		Con_ugm[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x]*as.numeric(name_df$MW/24.45016)[x-1]),ncol = ncol(df)-1))	
	}
	loh_df[,2:ncol(df)] = data.frame(matrix(sapply(2:ncol(df),function(x) df[,x] * 
		as.numeric(name_df$Koh*Avogadro*1e-12/24.45016)[x-1]),ncol = ncol(df)-1))
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
	loh_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes_exclude_BVOC=NA, BVOC=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
  }else{
	Con_ppbv_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA)
	Con_ugm_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA) 
	loh_df_group=data.frame(Time=df[,1], Alkanes=NA, Alkenes=NA, Alkynes=NA, Aromatic_Hydrocarbons=NA, Oxygenated_Organics=NA, Other_Organic_Compounds=NA, Unknown=NA) 
  }

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
  
  #remove NA columns
  Con_ppbv_group <- Con_ppbv_group[,colSums(is.na(Con_ppbv_group))<nrow(Con_ppbv_group)]
  Con_ugm_group <- Con_ugm_group[,colSums(is.na(Con_ugm_group))<nrow(Con_ugm_group)]
  loh_df_group <- loh_df_group[,colSums(is.na(loh_df_group))<nrow(loh_df_group)]
  
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

  #loh_df_stat
  loh_df_stat=data.frame(Species=row.names(statdf(loh_df)),Mean=as.numeric(as.character(statdf(loh_df,n = 3)[,1])),sd=as.numeric(as.character(statdf(loh_df,n = 3)[,2])),Min=as.numeric(as.character(statdf(loh_df,n = 3)[,3])),Q25=as.numeric(as.character(statdf(loh_df,n = 3)[,4])),Q50=as.numeric(as.character(statdf(loh_df,n = 3)[,5])),Q75=as.numeric(as.character(statdf(loh_df,n = 3)[,6])),Max=as.numeric(as.character(statdf(loh_df,n = 3)[,6])))
  loh_df_stat$Proportion=loh_df_stat$Mean/sum(as.numeric(as.character(statdf(loh_df,n = 3)[,1])),na.rm = TRUE)
  loh_df_stat$Proportion=round(loh_df_stat$Proportion,4)
  loh_df_stat=loh_df_stat[with(loh_df_stat, order(-Mean)), ]

  #Con_ugm_group_stat
  Con_ugm_group_stat=data.frame(Species=row.names(statdf(Con_ugm_group)),Mean=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,6])))
  Con_ugm_group_stat$Proportion=Con_ugm_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ugm_group,n = 3)[,1])),na.rm = TRUE)
  Con_ugm_group_stat$Proportion=round(Con_ugm_group_stat$Proportion,4)
  Con_ugm_group_stat=Con_ugm_group_stat[with(Con_ugm_group_stat, order(-Mean)), ]

  #Con_ppbv_group_stat
  Con_ppbv_group_stat=data.frame(Species=row.names(statdf(Con_ppbv_group)),Mean=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,6])))
  Con_ppbv_group_stat$Proportion=Con_ppbv_group_stat$Mean/sum(as.numeric(as.character(statdf(Con_ppbv_group,n = 3)[,1])),na.rm = TRUE)
  Con_ppbv_group_stat$Proportion=round(Con_ppbv_group_stat$Proportion,4)
  Con_ppbv_group_stat=Con_ppbv_group_stat[with(Con_ppbv_group_stat, order(-Mean)), ]
  
  #loh_df_group_stat
  loh_df_group_stat=data.frame(Species=row.names(statdf(loh_df_group)),Mean=as.numeric(as.character(statdf(loh_df_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(loh_df_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(loh_df_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(loh_df_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(loh_df_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(loh_df_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(loh_df_group,n = 3)[,6])))
  loh_df_group_stat$Proportion=loh_df_group_stat$Mean/sum(as.numeric(as.character(statdf(loh_df_group,n = 3)[,1])),na.rm = TRUE)
  loh_df_group_stat$Proportion=round(loh_df_group_stat$Proportion,4)
  loh_df_group_stat=loh_df_group_stat[with(loh_df_group_stat, order(-Mean)), ]
  
  #loh_df_group_stat
  loh_df_group_stat=data.frame(Species=row.names(statdf(loh_df_group)),Mean=as.numeric(as.character(statdf(loh_df_group,n = 3)[,1])),SD=as.numeric(as.character(statdf(loh_df_group,n = 3)[,2])),Min=as.numeric(as.character(statdf(loh_df_group,n = 3)[,3])),Q25=as.numeric(as.character(statdf(loh_df_group,n = 3)[,4])),Q50=as.numeric(as.character(statdf(loh_df_group,n = 3)[,5])),Q75=as.numeric(as.character(statdf(loh_df_group,n = 3)[,6])),Max=as.numeric(as.character(statdf(loh_df_group,n = 3)[,6])))
  loh_df_group_stat$Proportion=loh_df_group_stat$Mean/sum(as.numeric(as.character(statdf(loh_df_group,n = 3)[,1])),na.rm = TRUE)
  loh_df_group_stat$Proportion=round(loh_df_group_stat$Proportion,4)
  loh_df_group_stat=loh_df_group_stat[with(loh_df_group_stat, order(-Mean)), ]
  
  #results
  results <- list(
	KOH_Result = name_df,
	LOH_Result = loh_df,
	LOH_Result_stat = loh_df_stat,
	LOH_Result_group = loh_df_group,
	LOH_Result_group_stat = loh_df_group_stat
  )
  return(results)
}
