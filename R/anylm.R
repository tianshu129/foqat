#' Analysis of linear regression for time series in batch
#'
#' Analyse linear regression for time series in batch
#'
#' X axis, Y axis, scaled color for points are flexible for multiple columns.
#' Data could also be grouped according to 1 column. \cr
#'
#' @param df dataframe of time series.
#' @param xd species or columns for x axis, vector of number or colnames. Default vaule is '2'.
#' @param yd species or columns for y axis, vector of number or colnames. Default vaule is '3'.
#' @param zd species or columns to fill points, vector of number or colnames. Default vaule is 'NULL'.
#' If zd is setted, labels for scaled color represent Percentile value (0, 0.25, 0.5,0.75, 1).
#' @param td 1 column to group data, number or colname. Default vaule is 'NULL'.
#' @param mi index (1~4) of methods: 1. ordinary least squares (OLS); 2. major axis (MA); 3. standard major axis (SMA);
#' 4. and ranged major axis (RMA). Refered from R package 'lmodel2'. Default vaule is '1'.
#' @param range.y Parametres for ranged major axis regression (RMA). If range.y = NULL and range.x = NULL, RMA will not be computed.
#' If only one of them is NULL, the program will stop. If range.y = "relative": variable y has a true zero (relative-scale variable).
#' If range.y = "interval": variable y possibly includes negative values (interval-scale variable).
#' If range.x = "relative": variable x has a true zero (relative-scale variable).
#' If range.x = "interval": variable x possibly includes negative values (interval-scale variable). Refered from R package 'lmodel2'.
#' @param range.x Parametres, please see 'range.y'.
#' @param nperm Number of permutations for the tests. If nperm = 0, tests will not be computed. Refered from R package 'lmodel2'.
#' @param showpage logical value for showing all plots. If TRUE, print all plot in 1 page. Default vaule is 'TRUE'.
#' @param scint logical value for displaying scientific notion in legend and plot title. Default vaule is 'FALSE'.
#' @param dign numeric value for digists in legend and plot title. Default vaule is '1'.
#' @param zfill color for points, only valid when zd is NULL. Default vaule is "lightgray".
#' @param ppsize size for points. Default vaule is "lightgray".
#' @param showinfo logical value for displaying regression information in plot title. Default vaule is 'TRUE'.
#' @param ptsize font size for plot title. Default vaule is '12'.
#' @param pncol number of columns for plots in page. Refered from R package 'gridExtra'. Default vaule is 'NULL'.
#' @return a list contains: data_list, lm_df, lm_list, plot_list, all_plot.\cr
#' data_list: a list contains data for linear regression.\cr
#' lm_df: a dataframe for key results of linear regression. row index of lm_df corresponds to 'id' of plot in 'all_plot'.\cr
#' lm_list: a list contains detail results of linear regression.\cr
#' plot_list: a list contains plots for linear regression. \cr
#' all_plot: a page for all plots in 'plot_list'.\cr
#' To see page, please use this function: 'gridExtra::grid.arrange(grobs=...)'.\cr
#' To see page, please use this 2-lines function:\cr
#' 'g=gridExtra::arrangeGrob(grobs=...)',\cr
#' 'ggplot2::ggsave(filename = "example.jpg", plot =g)'.\cr
#' 'id' of plot corresponds to row index of 'lm_df'.\cr
#'
#' @export
#' @importFrom lmodel2 lmodel2
#' @importFrom ggplot2 ggplot aes_string geom_point ggtitle theme element_text aes scale_colour_gradientn
#' geom_abline scale_color_gradientn theme_bw ggplotGrob
#' @importFrom ggplotify as.ggplot
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices rainbow
#' @examples
#' anylm(aqi, xd=c(2,3), yd=6, zd=4, td=NULL, dign=3)

anylm <- function(df, xd=2, yd=3, zd=NULL, td=NULL, mi=1, range.y="interval", range.x="interval", nperm=99, showpage=TRUE, scint=FALSE, dign=1, zfill="lightgray", ppsize=2, showinfo=TRUE, ptsize=12, pncol=NULL){

  #function start################################
  #In case df is not a dataframe.
  df <- data.frame(df)

  #default vaules for zn & tn
  zn=0
  tn=0

  if(!is.numeric(xd)){
    xd=match(xd, names(df))
  }
  xn=length(xd)
  if(!is.numeric(yd)){
    yd=match(yd, names(df))
  }
  yn=length(yd)
  if(!is.null(zd)){
    if(!is.numeric(zd)){
      zd=match(zd, names(df))
    }
    zn=length(zd)
  }
  if(!is.null(td)){
    if(!is.numeric(td)){
      td=match(td, names(df))
    }
    tdsub=as.character(unique(df[,td]))
    tn=length(tdsub)
  }

  if(any(is.na(c(xd, yd, zd,td)))){
    stop("Please use same type (name/index) in xd/yd/zd/td")
  }

  #generate data_list
  data_list=list()
  for(kx in xd){
    y.list=list()
    for(ky in yd){
      if(!is.null(zd)&!is.null(td)){
        z.list=list()
        for(kz in zd){
          t.list=list()
          for(kt in 1:length(tdsub)){
            t.list[[tdsub[kt]]]=data.frame(df[df[,names(df)[td]]==tdsub[kt],c(kx,ky,kz,td)])
            colnames(t.list[[tdsub[kt]]])=c(names(df)[c(kx,ky,kz)],tdsub[kt])
          }
          z.list[[names(df)[kz]]]=t.list
        }
        y.list[[names(df)[ky]]]=z.list
      }else if(is.null(zd)&!is.null(td)){
        t.list=list()
        for(kt in 1:length(tdsub)){
          t.list[[tdsub[kt]]]=data.frame(df[df[,names(df)[td]]==tdsub[kt],c(kx,ky,td)])
          colnames(t.list[[tdsub[kt]]])=c(names(df)[c(kx,ky)],tdsub[kt])
        }
        y.list[[names(df)[ky]]]=t.list
      }else if(!is.null(zd)&is.null(td)){
        z.list=list()
        for(kz in zd){
          z.list[[names(df)[kz]]]=data.frame(df[,c(kx,ky,kz)])
          colnames(z.list[[names(df)[kz]]])=names(df)[c(kx,ky,kz)]
        }
        y.list[[names(df)[ky]]]=z.list
      }else{
        y.list[[names(df)[ky]]]=data.frame(df[,c(kx,ky)])
        colnames(y.list[[names(df)[ky]]])=names(df)[c(kx,ky)]
      }
    }
    data_list[[names(df)[kx]]]=y.list
  }

  #empty df for lr
  #x y z group r2 rma slope int P
  lm_df <- data.frame(matrix(ncol = 9, nrow = 0))
  x <- c("x", "y", "z", "group", "r2", "method", "slope", "int", "P")
  colnames(lm_df) <- x

  #generate lm_df & lm_list
  coli=0
  lm_list=list()
  for(xi in 1:xn){
    for(yi in 1:yn){
      if(zn==0&tn==0){
        #2 levels
        coli=coli+1
        tar=data_list[[xi]][[yi]]
        if(all(is.na(tar[,2]))|all(is.na(tar[,1]))){
          lm_df[coli,] <- rep(NA,9)
        }else{
          mod_2 <- lmodel2(tar[,2] ~ tar[,1], data = tar, range.y=range.y, range.x=range.x, nperm=nperm)
          lm_list[[coli]]<-mod_2
          mod_2_reg <- data.frame(mod_2$regression.results)
          mod_2_reg[,1]<-as.character(mod_2_reg[,1])
          lm_df[coli,] <- c(names(tar)[c(1,2)],NA,NA,mod_2[["rsquare"]],mod_2_reg[mi,c(1,3,2,5)])
        }
      }else if(zn!=0&tn==0){
        #3 levels
        for(zi in 1:zn){
          coli=coli+1
          tar=data_list[[xi]][[yi]][[zi]]
          if(all(is.na(tar[,2]))|all(is.na(tar[,1]))){
            lm_df[coli,] <- c(names(tar)[c(1,2,3)],NA,rep(NA,5))
          }else{
            mod_2 <- lmodel2(tar[,2] ~ tar[,1], data = tar, range.y=range.y, range.x=range.x, nperm=nperm)
            lm_list[[coli]]<-mod_2
            mod_2_reg <- data.frame(mod_2$regression.results)
            mod_2_reg[,1]<-as.character(mod_2_reg[,1])
            lm_df[coli,] <- c(names(tar)[c(1,2,3)],NA,mod_2[["rsquare"]],mod_2_reg[mi,c(1,3,2,5)])
          }
        }
      }else if(zn==0&tn!=0){
        #3 levels
        for(ti in 1:tn){
          coli=coli+1
          tar=data_list[[xi]][[yi]][[ti]]
          if(all(is.na(tar[,2]))|all(is.na(tar[,1]))){
            lm_df[coli,] <- c(names(tar)[c(1,2)],NA,names(tar)[3],rep(NA,5))
          }else{
            mod_2 <- lmodel2(tar[,2] ~ tar[,1], data = tar, range.y=range.y, range.x=range.x, nperm=nperm)
            lm_list[[coli]]<-mod_2
            mod_2_reg <- data.frame(mod_2$regression.results)
            mod_2_reg[,1]<-as.character(mod_2_reg[,1])
            lm_df[coli,] <- c(names(tar)[c(1,2)],NA,names(tar)[3],mod_2[["rsquare"]],mod_2_reg[mi,c(1,3,2,5)])
          }
        }
      }else if(zn!=0&tn!=0){
        #4 levels
        for(zi in 1:zn){
          for(ti in 1:tn){
            coli=coli+1
            tar=data_list[[xi]][[yi]][[zi]][[ti]]
            if(all(is.na(tar[,2]))|all(is.na(tar[,1]))){
              lm_df[coli,] <- c(names(tar)[c(1,2,3,4)],rep(NA,5))
            }else{
              mod_2 <- lmodel2(tar[,2] ~ tar[,1], data = tar, range.y=range.y, range.x=range.x, nperm=nperm)
              lm_list[[coli]]<-mod_2
              mod_2_reg <- data.frame(mod_2$regression.results)
              mod_2_reg[,1]<-as.character(mod_2_reg[,1])
              lm_df[coli,] <- c(names(tar)[c(1,2,3,4)],mod_2[["rsquare"]],mod_2_reg[mi,c(1,3,2,5)])
            }
          }
        }
      }
    }
  }
  lm_df[c(5,7:9)] <- sapply(lm_df[c(5,7:9)],as.numeric)


  #generate plot_list & all_plot
  plot_list_raw = list()
  coli=0
  for(xi in 1:xn){
    for(yi in 1:yn){
      if(zn==0&tn==0){
        #2 levels
        coli=coli+1
        tar=data_list[[xi]][[yi]]
        if(names(tar)[1]!=names(tar)[2]){
          if(showinfo==TRUE){
            rsqr=round(lm_df[coli,5], digits = 4)
            if(scint==TRUE){slp <- formatC(lm_df[coli,7], format = "e", digits = dign)}else{slp <- round(lm_df[coli,7], digits = dign)}
            title_text=paste0("id=", coli, " r2=", rsqr, " slp=", slp, collapse = NULL)
          }else{title_text=paste0("id=", coli, collapse = NULL)}
          p = ggplot(tar, aes_string(x=names(tar)[1], y=names(tar)[2])) + geom_point(size=ppsize, shape = 21, fill = zfill, color = "black") + geom_abline(intercept = lm_df[coli,8], slope = lm_df[coli,7], size=1) + ggtitle(title_text) + theme_bw() + theme(plot.title = element_text(size=ptsize))
          plot_list_raw[[coli]] = ggplotGrob(p)
        }
      }else if(zn!=0&tn==0){
        #3 levels
        for(zi in 1:zn){
          coli=coli+1
          tar=data_list[[xi]][[yi]][[zi]]
          if(names(tar)[1]!=names(tar)[2]){
            if(showinfo==TRUE){
              rsqr=round(lm_df[coli,5], digits = 4)
              if(scint==TRUE){slp <- formatC(lm_df[coli,7], format = "e", digits = dign)}else{slp <- round(lm_df[coli,7], digits = dign)}
              title_text=paste0("id=", coli, " r2=", rsqr, " slp=", slp, collapse = NULL)
            }else{title_text=paste0("id=", coli, collapse = NULL)}
            li <- c(min(tar[,3], na.rm=T),max(tar[,3], na.rm=T))
            la <- quantile(tar[,3], probs = c(0, 0.25, 0.5, 0.75, 1),na.rm=T)
            if(scint==TRUE){la <- formatC(la, format = "e", digits = dign)}else{la <- round(la, digits = dign)}
            br <- quantile(tar[,3], probs = c(0, 0.25, 0.5, 0.75, 1),na.rm=T)
            p = ggplot(tar, aes_string(x=names(tar)[1], y=names(tar)[2])) + geom_point(size=ppsize, aes(colour=tar[,3])) + scale_colour_gradientn(name=names(tar)[3], colours=rainbow(5), limits = li, labels = la, breaks = br) + geom_abline(intercept = lm_df[coli,8], slope = lm_df[coli,7], size=1) + ggtitle(title_text) + theme_bw() + theme(plot.title = element_text(size=ptsize))
            plot_list_raw[[coli]]=ggplotGrob(p)
          }
        }
      }else if(zn==0&tn!=0){
        #3 levels
        for(ti in 1:tn){
          coli=coli+1
          tar=data_list[[xi]][[yi]][[ti]]
          tar[,3]=as.factor(as.character(tar[,3]))
          if(names(tar)[1]!=names(tar)[2]){
            if(showinfo==TRUE){
              rsqr=round(lm_df[coli,5], digits = 4)
              if(scint==TRUE){slp <- formatC(lm_df[coli,7], format = "e", digits = dign)}else{slp <- round(lm_df[coli,7], digits = dign)}
              title_text=paste0("id=", coli, " r2=", rsqr, " slp=", slp, " gp=", as.character(tdsub[ti]), collapse = NULL)
            }else{title_text=paste0("id=", coli, collapse = NULL)}
            p = ggplot(tar, aes_string(x=names(tar)[1], y=names(tar)[2])) + geom_point(size=ppsize, shape = 21, fill = zfill, color = "black") + geom_abline(intercept = lm_df[coli,8], slope = lm_df[coli,7], size=1) + ggtitle(title_text) + theme_bw() + theme(plot.title = element_text(size=ptsize))
            plot_list_raw[[coli]]=ggplotGrob(p)
          }
        }
      }else if(zn!=0&tn!=0){
        #4 levels
        for(zi in 1:zn){
          for(ti in 1:tn){
            coli=coli+1
            tar=data_list[[xi]][[yi]][[zi]][[ti]]
            if(names(tar)[1]!=names(tar)[2]){
              if(showinfo==TRUE){
                rsqr=round(lm_df[coli,5], digits = 4)
                if(scint==TRUE){slp <- formatC(lm_df[coli,7], format = "e", digits = dign)}else{slp <- round(lm_df[coli,7], digits = dign)}
                title_text=paste0("id=", coli, " r2=", rsqr, " slp=", slp, " gp=", as.character(tdsub[ti]), collapse = NULL)
              }else{title_text=paste0("id=", coli, collapse = NULL)}
              li <- c(min(tar[,3], na.rm=T),max(tar[,3], na.rm=T))
              la <- quantile(tar[,3], probs = c(0, 0.25, 0.5, 0.75, 1),na.rm=T)
              if(scint==TRUE){la <- formatC(la, format = "e", digits = dign)}else{la <- round(la, digits = dign)}
              br <- quantile(tar[,3], probs = c(0, 0.25, 0.5, 0.75, 1),na.rm=T)
              p = ggplot(tar, aes_string(x=names(tar)[1], y=names(tar)[2])) + geom_point(size=ppsize, aes(colour=tar[,3])) + scale_color_gradientn(name=names(tar)[3], colours=rainbow(10), limits = li, labels = la, breaks = br) + geom_abline(intercept = lm_df[coli,8], slope = lm_df[coli,7], size=1) + ggtitle(title_text) + theme_bw() + theme(plot.title = element_text(size=ptsize))
              plot_list_raw[[coli]]=ggplotGrob(p)
            }
          }
        }
      }
    }
  }

  #save all_plot
  all_plot=plot_list_raw[which(!is.na(lm_df$r2))]

  #show all_plot
  if(showpage==TRUE){grid.arrange(grobs=all_plot, ncol=pncol)}

  plot_list=list()
  for(pi in 1:length(plot_list_raw)){
    plot_list[[pi]]=as.ggplot(plot_list_raw[[pi]])
  }

  #generate final output
  result<-list(data_list=data_list, lm_df=lm_df, lm_list=lm_list, plot_list=plot_list, all_plot=all_plot)
  return(result)
  #function end################################
}



