#' Calculate TUV Online
#'
#' This function runs TUV online by reading the input parameters, and summarizes the results to the new dataframe. \cr
#'
#' @param inputMode The default value is 0. InputMode 0: User-specified geographic location and time/date. The code computes the appropriate solar zenith angle and Earth-Sun distance.  InputMode 1: User specifies the solar zenith angle, and the annual average Earth-Sun distance is used. To avoid inconsistencies (e.g. overhead sun at the poles), options 1 and 2 cannot be invoked at the same time.   
#' @param outputMode The default value is 2.  OutputMode 2: Molecular photolysis frequencies (109 photoreactions). OutputMode 3: Weighted irradiance (27 weighting functions). OutputMode 4: Spectral actinic flux. OutputMode 5: Spectral irradiance.
#' @param nStreams The default value is -2.   NStreams -2: Pseudo-spherical 2 streams (faster, less accurate). NStreams 4: Pseudo-spherical discrete ordinate 4 streams (slower, more accurate).
#' @param wStart Shortest wavelength. The default value is 280. \cr  
#' @param wStop Longest wavelength.  The default value is 420. \cr
#' @param wIntervals Number of equal-sized subdivisions of the range End-Start. The default value is 140. \cr 
#' @param latitude Latitudes: positive North of equator, negative South of equator. The default value is 0. \cr
#' @param longitude Longitudes: positive East of the Greenwich meridian, negative West of the Greenwich meridian. The default value is 0. \cr  
#' @param zenith Solar zenith angle (deg). The default value is 0. \cr
#' @param ozone Ozone column, in Dobson Units (du), vertical, from ground (even if above sea level) to space. The US Standard Atmosphere O3 is used to specify the shape of the vertical profile but the total column is re-scaled to the value selected here by the user. The default value is 300. \cr
#' @param albedo Surface albedo: Assumes a Lambertian reflection (isotropic radiance) Values for snow can reach 0.90-0.99, but otherwise values at UV wavelengths are in the range 0.02-0.20 depending on the precise surface.  The default value is 0.1.
#' @param gAltitude Ground elevation: The elevation of the ground, in km above mean sea level.  The default value is 0. \cr
#' @param mAltitude Measurement altitude: The altitude in the atmosphere for which results are requested. This should not be confused with the ground elevation. For example, if you have measurements made from an airplane, flying at 6 km above the ground, and the surface is at 1.5 km, then you will want to request results for a measurement altitude of 7.5 km asl.   The default value is 0. \cr
#' @param taucld Cloud Optical Depth: vertical optical depth of the cloud. The default value is 0.00.  \cr
#' @param zbase Cloud base: base of cloud, in km (asl). The default value is 4.00. \cr  
#' @param ztop Cloud top:  top of cloud, in km (asl). The default value is 5.00. \cr  
#' @param tauaer Optical Depth: total extinction (absorption + scattering) at 550 nm, vertical, from ground to space. The default value is 0.235. \cr
#' @param ssaaer Single Scattering Albedo (S-S alb), assumed independent of wavelength. The default value is 0.990. \cr
#' @param alpha Alpha (Angstrom exponent), gives wavelength dependence of optical depth, by multiplying the 550 nm value by (550 nm/wavelength, nm)**alpha. The default value is 1.000. \cr
#' @param dirsun Direct beam, direct solar beam. The default value is 1.0. \cr 
#' @param difdn Diffuse down, down-ward propagating scattered radiation (diffuse sky light). The default value is 1.0. \cr  
#' @param difup Diffuse up, up-ward propagating scattered radiation (diffuse light from below). The default value is NA. \cr  
#' @param date Date (format: (YYYYMMDD, GMT). The default value is 20150630. \cr  
#' @param timeStamp -> Timestamp (format: hh:mm:ss, GMT). The default value is "12:00:00". \cr 
#' @param time Hour. The default value is 12. \cr 
#' @return a dataframe. The contents of dataframe are diterminated by OutputMode. \cr 
#' OutputMode 2: Molecular photolysis frequencies (109 photoreactions). \cr
#' OutputMode 3: Weighted irradiance (27 weighting functions). \cr
#' OutputMode 4: Spectral actinic flux. \cr
#' OutputMode 5: Spectral irradiance. \cr

tuv_core=function(wStart=280, wStop=420, wIntervals=140, inputMode=0, latitude=0, longitude=0, date=20150630, timeStamp="12:00:00",  zenith=0, ozone=300, albedo=0.1, gAltitude=0, mAltitude=0, taucld=0.00, zbase=4.00, ztop=5.00, tauaer=0.235, ssaaer=0.990, alpha=1.000, time=12, outputMode=2, nStreams=-2, dirsun=1.0, difdn=1.0, difup=NA){
  if(is.na(difup)&(outputMode==2|outputMode==4)){
	difup=1.0
  }else if(is.na(difup)&(outputMode==3|outputMode==5)){
	difup=0.0
  }
  url <- paste0(c("https://www.acom.ucar.edu/cgi-bin/acom/TUV/V5.3/tuv?wStart=", wStart, "&wStop=", wStop, "&wIntervals=", wIntervals, "&inputMode=", inputMode, "&latitude=", latitude, "&longitude=", longitude, "&date=", date, "&timeStamp=", timeStamp,  "&zenith=", zenith, "&ozone=", ozone, "&albedo=", albedo, "&gAltitude=", gAltitude, "&mAltitude=", mAltitude, "&taucld=", taucld, "&zbase=", zbase, "&ztop=", ztop, "&tauaer=", tauaer, "&ssaaer=", ssaaer, "&alpha=", alpha, "&time=", time, "&outputMode=", outputMode, "&nStreams=", nStreams, "&dirsun=", dirsun, "&difdn=", difdn, "&difup=", difup), collapse='')
  download.file(url, "file.txt", quiet=TRUE)
  filetext=read.delim("file.txt")
  if(outputMode==2){
	#PHOTOLYSIS RATES (1/sec)
	photolysis_rates=filetext[23:135,]
	phlydf=do.call(rbind.data.frame,strsplit(photolysis_rates, "\\s{4,}"))
	phlydf[34,1]=paste0(" ", strsplit(filetext[56,], "\\s{2,}")[[1]][2])
	phlydf[34,2]=strsplit(filetext[56,], "\\s{2,}")[[1]][3]
	phlydf_value=data.frame(t(phlydf[,2]))
	names(phlydf_value)=phlydf[,1]
	return(phlydf_value)
  }else if(outputMode==3){
	#WEIGHTED IRRADIANCES (W m-2)
	weighted_irradiances=filetext[24:27,]
	weirdf=do.call(rbind.data.frame,strsplit(weighted_irradiances, "\\s{2,}"))
	weirdf_value=data.frame(t(weirdf[,3]))
	names(weirdf_value)=weirdf[,2]
	return(weirdf_value)
  }else if(outputMode==4){
	#ACTINIC FLUX (# photons/sec/nm/cm2)
	actinic_flux=filetext[24:163,]
	acfldf_value=do.call(rbind.data.frame,strsplit(actinic_flux, "\\s{2,}"))
	names(acfldf_value)=unlist(strsplit(filetext[23,], "\\s{2,}"))
	return(acfldf_value)
  }else if(outputMode==5){
	#SPECTRAL IRRADIANCE (W m-2 nm-1)
	spectral_irradiance=filetext[24:163,]
	spirdf_value=do.call(rbind.data.frame,strsplit(spectral_irradiance, "\\s{2,}"))
	names(spirdf_value)=unlist(strsplit(filetext[23,], "\\s{2,}"))
	return(spirdf_value)
  }
}