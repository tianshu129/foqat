#' Calculate Surface Area, Volume, Mass of particle by particle number concentration
#'
#' Calculate Surface Area, Volume, Mass of particle by particle number concentration.
#'
#' @param df dataframe of particle size data: the first column of input is datetime; the other columns are number concentration (N, unit: #/cm3) or log number concentration (dN/dlogdp, unit: #/cm3) for each particle size channel. Column names of the other columns are the middle particle size for each particle size channel.  
#' @param dlogdp logical value, TRUE if the third column is log number concentration (dN/dlogdp).
#' @param dsty numeric value, density of particle namtter.
#' @return a list with 2 dataframe. The first dataframe is a time series for Surface Area (unit: µm2/cm3), Volume (unit: µm3/cm3), Mass  (unit: µg/m3) of each channels; the second dataframe is a time series for total Surface Area, Volume, Mass of all channels. 

#' @export
#' @importFrom stats na.omit


nsvp <- function(df,dlogdp=FALSE, dsty=1){
	#trans from table to list
	df=transp(df)
	
	chlist=sort(na.omit(unique(df[,2])))
	dlogdp_value=mean(log10(chlist[2:length(chlist)])-log10(chlist[1:(length(chlist)-1)]))
	
	if(dlogdp==TRUE){
	df[,4]=df[,3]*dlogdp_value#dN
	df[,c(3,4)]=df[,c(4,3)]
	}else{
	df[,4]=df[,3]/dlogdp_value#dN_dlogdp
	}
	names(df)[c(3,4)]=c("dN","dN_dlogdp")
	df$dS=pi*((df[,2]*0.001)^2)*df[,3]
	df$dV=(pi/6)*((df[,2]*0.001)^3)*df[,3]
	df$dM=df$dV*dsty
	df$dS_dlogdp=df$dS/dlogdp_value
	df$dV_dlogdp=df$dV/dlogdp_value
	df$dM_dlogdp=df$dM/dlogdp_value
	ndf=transp(df[,c(1,2,3)])
	sdf=transp(df[,c(1,2,4)])
	vdf=transp(df[,c(1,2,5)])
	mdf=transp(df[,c(1,2,6)])
	sum_df=data.frame(Datetime=ndf[,1],N=rowSums(ndf[,-1],na.rm=TRUE),S=rowSums(sdf[,-1],na.rm=TRUE),V=rowSums(vdf[,-1],na.rm=TRUE),M=rowSums(mdf[,-1],na.rm=TRUE))
	
	#output
	results <- list(df_channels = df, df_total = sum_df)
	return(results)	
}
