#' Calculate TUV in Batch Online
#'
#' This function runs TUV in batch online by reading the time series for the \cr
#' parameters to be entered, and summarizes the results to the new dataframe. \cr
#'
#' @param df Dataframe of time series of parameters. The first column of df should be datetime. The other columns (names) could be set as following: \cr  
#' wStart -> Shortest wavelength. The default value is 280. \cr  
#' wStop -> Longest wavelength.  The default value is 420. \cr
#' wIntervals -> Number of equal-sized subdivisions of the range End-Start. The default value is 140. \cr 
#' latitude -> Latitudes: positive North of equator, negative South of equator. The default value is 0. \cr
#' longitude -> Longitudes: positive East of the Greenwich meridian, negative West of the Greenwich meridian. The default value is 0. \cr  
#' zenith -> Solar zenith angle (deg). The default value is 0. \cr
#' ozone -> Ozone column, in Dobson Units (du), vertical, from ground (even if above sea level) to space. The US Standard Atmosphere O3 is used to specify the shape of the vertical profile but the total column is re-scaled to the value selected here by the user. The default value is 300. \cr
#' albedo -> Surface albedo: Assumes a Lambertian reflection (isotropic radiance) Values for snow can reach 0.90-0.99, but otherwise values at UV wavelengths are in the range 0.02-0.20 depending on the precise surface.  The default value is 0.1.
#' gAltitude -> Ground elevation: The elevation of the ground, in km above mean sea level.  The default value is 0. \cr
#' mAltitude -> Measurement altitude: The altitude in the atmosphere for which results are requested. This should not be confused with the ground elevation. For example, if you have measurements made from an airplane, flying at 6 km above the ground, and the surface is at 1.5 km, then you will want to request results for a measurement altitude of 7.5 km asl.   The default value is 0. \cr
#' taucld -> Cloud Optical Depth: vertical optical depth of the cloud. The default value is 0.00.  \cr
#' zbase -> Cloud base: base of cloud, in km (asl). The default value is 4.00. \cr  
#' ztop -> Cloud top:  top of cloud, in km (asl). The default value is 5.00. \cr  
#' tauaer -> Optical Depth: total extinction (absorption + scattering) at 550 nm, vertical, from ground to space. The default value is 0.235. \cr
#' ssaaer -> Single Scattering Albedo (S-S alb), assumed independent of wavelength. The default value is 0.990. \cr
#' alpha -> Alpha (Angstrom exponent), gives wavelength dependence of optical depth, by multiplying the 550 nm value by (550 nm/wavelength, nm)**alpha. The default value is 1.000. \cr
#' dirsun -> Direct beam, direct solar beam. The default value is 1.0. \cr 
#' difdn -> Diffuse down, down-ward propagating scattered radiation (diffuse sky light). The default value is 1.0. \cr  
#' difup -> Diffuse up, up-ward propagating scattered radiation (diffuse light from below). The default value is NA. \cr  
#' @param inputMode The default value is 0. InputMode 0: User-specified geographic location and time/date. The code computes the appropriate solar zenith angle and Earth-Sun distance.  InputMode 1: User specifies the solar zenith angle, and the annual average Earth-Sun distance is used. To avoid inconsistencies (e.g. overhead sun at the poles), options 1 and 2 cannot be invoked at the same time.   
#' @param outputMode The default value is 2.  OutputMode 2: Molecular photolysis frequencies (109 photoreactions). OutputMode 3: Weighted irradiance (27 weighting functions). OutputMode 4: Spectral actinic flux. OutputMode 5: Spectral irradiance.
#' @param nStreams The default value is -2.   NStreams -2: Pseudo-spherical 2 streams (faster, less accurate). NStreams 4: Pseudo-spherical discrete ordinate 4 streams (slower, more accurate).
#' @return a dataframe. The contents of dataframe are diterminated by OutputMode. \cr 
#' OutputMode 2: Molecular photolysis frequencies (109 photoreactions). \cr
#' OutputMode 3: Weighted irradiance (27 weighting functions). \cr
#' OutputMode 4: Spectral actinic flux. \cr
#' OutputMode 5: Spectral irradiance. \cr
#'
#' @export
#' @importFrom lubridate hour minute second
#' @importFrom utils read.delim setTxtProgressBar txtProgressBar

tuv_batch=function(df, inputMode=0, outputMode=2, nStreams=-2){
	#转化为data.frame
	df=data.frame(df)

	#检测输入的列名是否在范围内

	#date和timeStamp从第一列自动拆分生成 
	#time根据从第一列的小时数换算成带小数点的小时数据，也自动生成
	df$date=format(as.Date(df[,1]), "%Y%m%d")
	df$timeStamp=as.character(format(df[,1], "%H:%M:%S"))
	df$time=hour(df[,1])+minute(df[,1])/60+second(df[,1])/3600

	#补齐df
	if(outputMode==2|outputMode==4){
		allpara=c(280, 420, 140, 0, 0, 0, 300, 0.1, 0, 0, 0.00, 4.00, 5.00, 0.235, 0.990, 1.000, 1.0, 1.0, 1.0)
	}else{
		allpara=c(280, 420, 140, 0, 0, 0, 300, 0.1, 0, 0, 0.00, 4.00, 5.00, 0.235, 0.990, 1.000, 1.0, 1.0, 0.0)
	}
	names(allpara)=c("wStart", "wStop", "wIntervals", "latitude", "longitude", "zenith", "ozone", "albedo", "gAltitude", "mAltitude", "taucld", "zbase", "ztop", "tauaer", "ssaaer", "alpha", "dirsun", "difdn", "difup")

	#如果存在allpara有不在names(df)
	if(any(!allpara%in%names(df))){
		for(i in names(allpara)[!names(allpara)%in%names(df)]){
			eval(parse(text=paste0("df$'", i, "'=allpara[['", i, "']]")))
		}
	}

	#inputMode和outputMode从数据参数硬补
	df$"inputMode"=inputMode
	df$"outputMode"=outputMode

	#nStreams从数据参数硬补
	df$"nStreams"=nStreams

	#进度条
	print("Running TUV on the inputs:")
	progress_bar = txtProgressBar(min=0, max=nrow(df)+1, style = 3, char="=")
	 
	#对df按行输入tuv_core计算
	#先计算第一行。
	wStart=df$wStart[1]
	wStop=df$wStop[1]
	wIntervals=df$wIntervals[1]
	inputMode=df$inputMode[1]
	latitude=df$latitude[1]
	longitude=df$longitude[1]
	date=df$date[1]
	timeStamp=df$timeStamp[1]
	zenith=df$zenith[1]
	ozone=df$ozone[1]
	albedo=df$albedo[1]
	gAltitude=df$gAltitude[1]
	mAltitude=df$mAltitude[1]
	taucld=df$taucld[1]
	zbase=df$zbase[1]
	ztop=df$ztop[1]
	tauaer=df$tauaer[1]
	ssaaer=df$ssaaer[1]
	alpha=df$alpha[1]
	time=df$time[1]
	outputMode=df$outputMode[1]
	nStreams=df$nStreams[1]
	dirsun=df$dirsun[1]
	difdn=df$difdn[1]
	difup=df$difup[1]

	final_df=tuv_core(wStart=wStart, wStop=wStop, wIntervals=wIntervals, inputMode=inputMode, latitude=latitude, longitude=longitude, date=date, timeStamp=timeStamp,  zenith=zenith, ozone=ozone, albedo=albedo, gAltitude=gAltitude, mAltitude=mAltitude, taucld=taucld, zbase=zbase, ztop=ztop, tauaer=tauaer, ssaaer=ssaaer, alpha=alpha, time=time, outputMode=outputMode, nStreams=nStreams, dirsun=dirsun, difdn=difdn, difup=difup)
	setTxtProgressBar(progress_bar, value = 1)

	#如果有第二行，再循环计算，并拼接。
	if(nrow(df)>1){
		for(i in 2:nrow(df)){
			
			wStart=df$wStart[i]
			wStop=df$wStop[i]
			wIntervals=df$wIntervals[i]
			inputMode=df$inputMode[i]
			latitude=df$latitude[i]
			longitude=df$longitude[i]
			date=df$date[i]
			timeStamp=df$timeStamp[i]
			zenith=df$zenith[i]
			ozone=df$ozone[i]
			albedo=df$albedo[i]
			gAltitude=df$gAltitude[i]
			mAltitude=df$mAltitude[i]
			taucld=df$taucld[i]
			zbase=df$zbase[i]
			ztop=df$ztop[i]
			tauaer=df$tauaer[i]
			ssaaer=df$ssaaer[i]
			alpha=df$alpha[i]
			time=df$time[i]
			outputMode=df$outputMode[i]
			nStreams=df$nStreams[i]
			dirsun=df$dirsun[i]
			difdn=df$difdn[i]
			difup=df$difup[i]
			final_df_sub=tuv_core(wStart=wStart, wStop=wStop, wIntervals=wIntervals, inputMode=inputMode, latitude=latitude, longitude=longitude, date=date, timeStamp=timeStamp,  zenith=zenith, ozone=ozone, albedo=albedo, gAltitude=gAltitude, mAltitude=mAltitude, taucld=taucld, zbase=zbase, ztop=ztop, tauaer=tauaer, ssaaer=ssaaer, alpha=alpha, time=time, outputMode=outputMode, nStreams=nStreams, dirsun=dirsun, difdn=difdn, difup=difup)
			final_df=rbind(final_df, final_df_sub)
			setTxtProgressBar(progress_bar, value = i)
		}
	}
	
	#补信息列，时间列
	if(outputMode==2){
		#PHOTOLYSIS RATES (1/sec)
		info=rep("1/sec",nrow(final_df))
		final_df2=cbind(info,final_df)
		final_df3=cbind(df[,1],final_df2)
		names(final_df3)[c(1,2)]=c(names(df)[1],"PHOTOLYSIS RATES")
	}else if(outputMode==3){
		#WEIGHTED IRRADIANCES (W m-2)
		info=rep("W m-2",nrow(final_df))
		final_df2=cbind(info,final_df)
		final_df3=cbind(df[,1],final_df2)
		names(final_df3)[c(1,2)]=c(names(df)[1],"WEIGHTED IRRADIANCES")	
	}else if(outputMode==4){
		#ACTINIC FLUX (# photons/sec/nm/cm2)
		info=rep("photons/sec/nm/cm2",nrow(final_df))
		final_df2=cbind(info,final_df)
		final_df3=cbind(rep(df[,1], each=140),final_df2)
		names(final_df3)[c(1,2)]=c(names(df)[1],"ACTINIC FLUX")	
	}else if(outputMode==5){
		#SPECTRAL IRRADIANCE (W m-2 nm-1)
		info=rep("W m-2 nm-1",nrow(final_df))
		final_df2=cbind(info,final_df)
		final_df3=cbind(rep(df[,1], each=140),final_df2)
		names(final_df3)[c(1,2)]=c(names(df)[1],"SPECTRAL IRRADIANCE")	
	}
	
	setTxtProgressBar(progress_bar, value = nrow(df)+1)
	close(progress_bar)
	
	return(final_df3)
}