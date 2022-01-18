#' Plot time series
#'
#' Easy way to plot time series.
#' 
#' @param df dataframe contains time series.
#' @param yl vector, col index of species to be putted in the left y axis.
#' @param yr vector, col index of species to be putted in the right y axis. The default vaule is NULL.
#' @param yllab text expression of left y axis label. The default vaule is NULL.
#' @param yrlab text expression of right y axis label. The default vaule is NULL.
#' @param xlab text expression of x axis label. The default vaule is NULL.
#' @param llist vector, col index of species to be ploted by line.The default vaule is NULL.
#' @param plist vector, col index of species to be ploted by points.The default vaule is NULL.
#' @param alist plist vector, col index of species to be ploted by areas. The default vaule is NULL.
#' @param blist plist vector, col index of species to be ploted by bars. The default vaule is NULL.
#' @param llab text expression of legend labels of lines. The default vaule is NULL.
#' @param plab text expression of legend labels of points. The default vaule is NULL.
#' @param alab text expression of legend labels of areas. The default vaule is NULL.
#' @param blab text expression of legend labels of bars. The default vaule is NULL.
#' @param ltype vector, type of lines. The default vaule is NULL.
#' @param pshape vector, shape of points. The default vaule is NULL.
#' @param lsize vector, size of lines. The default vaule is NULL.The default vaule is 1.
#' @param psize vector, size of points. The default vaule is NULL.The default vaule is 1.
#' @param lcc vector, colors of lines. The default vaule is NULL. The default vaule is NULL.
#' @param pcc vector, colors of points. The default vaule is NULL. The default vaule is NULL.
#' @param aff fill color of areas. The default vaule is NULL.
#' @param bff fill color of bars. The default vaule is NULL.
#'
#' @export
#' @examples 
#' aqi2=aqi
#' aqi2$NO[aqi2$NO>7]=NA
#' aqi2$NO2=aqi2$NO2*0.3
#' geom_ts(
#'     	df=aqi2,
#'     	yl=c(3,2), 
#'     	yr=6, 
#'     	alist=c(3,2), 
#'     	llist=6, 
#'    	alab=list(bquote(NO[2]~" "), bquote(NO~" ")),
#' 		llab=list(bquote(O[3]~" ")),
#'      yllab=bquote(NO[x]~" "~(ppbv)),
#'      yrlab=bquote(O[3]~" "~(ppbv)),
#'      lcc="#ff4d4f", 
#'      aff=c("#096dd9","#69c0ff"),
#' xlab="Datetime")
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_area geom_bar geom_point geom_line scale_fill_discrete scale_fill_manual scale_linetype_manual scale_color_discrete scale_color_manual scale_shape_manual scale_y_continuous scale_x_datetime theme
#' @importFrom ggnewscale new_scale_color new_scale_fill 
#' @importFrom stats setNames

geom_ts <- function(df, 
yl=NULL, yr=NULL, 
yllab=NULL, yrlab=NULL, xlab=NULL,
llist=NULL, plist=NULL, alist=NULL, blist=NULL, 
llab=NULL, plab=NULL, alab=NULL, blab=NULL,
ltype=NULL, pshape=NULL, lsize=1, psize=1,
lcc=NULL, pcc=NULL, aff=NULL, bff=NULL){
	#命名时间列#################################
	names(df)[1]="Datetime"
	
	#副轴数值范围确定#################################
	ryl=abs(max(df[,yl],na.rm = TRUE)-min(df[,yl],na.rm = TRUE))
	if(length(yr)!=0){
		ryr=abs(max(df[,yr],na.rm = TRUE)-min(df[,yr],na.rm = TRUE))
	}

	#初始化图层#################################
	p = ggplot()

	#绘制面图层#################################

	p=p+new_scale_color()+new_scale_fill()

	##if area in left
	if(length(intersect(alist,yl))!=0){
		###右Y和面积型交集列号
		yl_alist=intersect(alist,yl)
		###用右Y和面积型交集列号从df取子数据集
		df_yl_alist=df[,c(1,yl_alist)]
		###为因子排序提前提取交集物种名
		fc_yl_alist=names(df_yl_alist)[-1]
		###子数据集变形以供画图
		df_yl_alist=melt(df_yl_alist, id.vars = names(df_yl_alist)[1])	
		###因子排序
		df_yl_alist$variable=factor(df_yl_alist$variable,levels=fc_yl_alist)
		###NA值用0
		df_yl_alist[is.na(df_yl_alist)]=0
		p=p+geom_area(data=df_yl_alist, aes(x = Datetime,y = value, fill = variable), position = 'stack')
	}

	##if area in right
	if(length(intersect(alist,yr))!=0){
		###左Y和面积型交集列号
		yr_alist=intersect(alist,yr)
		###用左Y和面积型交集列号从df取子数据集
		df_yr_alist=df[,c(1,yr_alist)]
		###为因子排序提前提取交集物种名
		fc_yr_alist=names(df_yr_alist)[-1]
		###子数据集变形以供画图
		df_yr_alist=melt(df_yr_alist, id.vars = names(df_yr_alist)[1])
		###因子排序
		df_yr_alist$variable=factor(df_yr_alist$variable,levels=fc_yr_alist)
		###NA值用0
		df_yr_alist[is.na(df_yr_alist)]=0
		p=p+geom_area(data=df_yr_alist, aes(x = Datetime,y = value*ryl/ryr, fill = variable), position = 'stack')
	}

	#if(!exists("yl_alist")){yl_alist=NULL}
	#if(!exists("yr_alist")){yr_alist=NULL}

	##限制legend
	if(length(alab)==0){alab=names(df)[alist]}
	labs=setNames(alab,names(df)[alist])

	##判断填充需求
	if(length(aff)==0){
		p = p + scale_fill_discrete(labels=labs)
	}else{
		cols=setNames(aff,names(df[,alist]))
		p = p + scale_fill_manual(values=cols, labels=labs)
	}

	#绘制柱图层#################################

	p=p+new_scale_color()+new_scale_fill()

	##if area in left
	if(length(intersect(blist,yl))!=0){
		###右Y和面积型交集列号
		yl_blist=intersect(blist,yl)
		###用右Y和面积型交集列号从df取子数据集
		df_yl_blist=df[,c(1,yl_blist)]
		###为因子排序提前提取交集物种名
		fc_yl_blist=names(df_yl_blist)[-1]
		###子数据集变形以供画图
		df_yl_blist=melt(df_yl_blist, id.vars = names(df_yl_blist)[1])	
		###因子排序
		df_yl_blist$variable=factor(df_yl_blist$variable,levels=fc_yl_blist)
		###NA值用0
		df_yl_blist[is.na(df_yl_blist)]=0
		p=p+geom_bar(data=df_yl_blist, aes(x = Datetime,y = value, fill = variable), stat = 'identity')
	}

	##if area in right
	if(length(intersect(blist,yr))!=0){
		###左Y和面积型交集列号
		yr_blist=intersect(blist,yr)
		###用左Y和面积型交集列号从df取子数据集
		df_yr_blist=df[,c(1,yr_blist)]
		###为因子排序提前提取交集物种名
		fc_yr_blist=names(df_yr_blist)[-1]
		###子数据集变形以供画图
		df_yr_blist=melt(df_yr_blist, id.vars = names(df_yr_blist)[1])
		###因子排序
		df_yr_blist$variable=factor(df_yr_blist$variable,levels=fc_yr_blist)
		###NA值用0
		df_yr_blist[is.na(df_yr_blist)]=0
		p=p+geom_bar(data=df_yr_blist, aes(x = Datetime,y = value*ryl/ryr, fill = variable), stat = 'identity')
	}

	#if(!exists("yl_blist")){yl_blist=NULL}
	#if(!exists("yr_blist")){yr_blist=NULL}

	##限制legend
	if(length(blab)==0){blab=names(df)[blist]}
	labs=setNames(blab,names(df)[blist])

	##判断填充需求
	if(length(bff)==0){
		p = p + scale_fill_discrete(labels=labs)
	}else{
		cols=setNames(bff,names(df[,blist]))
		p = p + scale_fill_manual(values=cols, labels=labs)
	}


	#绘制线图层#################################

	p=p+new_scale_color()+new_scale_fill()

	##if line in left
	if(length(intersect(llist,yl))!=0){
		###左Y和线型交集列号
		yl_llist=intersect(llist,yl)
		###用左Y和线型交集列号从df取子数据集
		df_yl_llist=df[,c(1,yl_llist)]
		###为因子排序提前提取交集物种名
		fc_yl_llist=names(df_yl_llist)[-1]
		###子数据集变形以供画图
		df_yl_llist=melt(df_yl_llist, id.vars = names(df_yl_llist)[1])
		###因子排序
		df_yl_llist$variable=factor(df_yl_llist$variable,levels=fc_yl_llist)
		###
		if(length(ltype)==0){
			p=p+geom_line(data=df_yl_llist, aes(x=df_yl_llist[,1], y=value, color=variable), size=lsize)
		}else{
			p=p+geom_line(data=df_yl_llist, aes(x=df_yl_llist[,1], y=value, color=variable, linetype=variable), size=lsize)
		}
	}

	##if line in right
	if(length(intersect(llist,yr))!=0){
		###右Y和线型交集列号
		yr_llist=intersect(llist,yr)
		###用右Y和线型交集列号从df取子数据集
		df_yr_llist=df[,c(1,yr_llist)]
		###为因子排序提前提取交集物种名
		fc_yr_llist=names(df_yr_llist)[-1]
		###子数据集变形以供画图
		df_yr_llist=melt(df_yr_llist, id.vars = names(df_yr_llist)[1])
		###因子排序
		df_yr_llist$variable=factor(df_yr_llist$variable,levels=fc_yr_llist)
		if(length(ltype)==0){
			p=p+geom_line(data=df_yr_llist, aes(x=df_yr_llist[,1], y=value*ryl/ryr, color=variable), size=lsize)
		}else{
			p=p+geom_line(data=df_yr_llist, aes(x=df_yr_llist[,1], y=value*ryl/ryr, color=variable, linetype=variable), size=lsize)
		}
	}

	#if(!exists("yl_llist")){yl_llist=NULL}
	#if(!exists("yr_llist")){yr_llist=NULL}

	##限制legend
	if(length(llab)==0){llab=names(df)[llist]}
	labs=setNames(llab,names(df)[llist])

	##判断线型需求
	##判断线型需求
	if(length(ltype)!=0){
	p = p + scale_linetype_manual(values=ltype, labels=labs)
	} 

	##判断颜色需求
	if(length(lcc)==0){
		p = p + scale_color_discrete(labels=labs)
	}else{
		cols=setNames(lcc,names(df[,llist]))
		p = p + scale_color_manual(values=cols, labels=labs)
	}


	#绘制点图层#################################

	p=p+new_scale_color()+new_scale_fill()

	##if point in left
	if(length(intersect(plist,yl))!=0){
		###左Y和点型交集列号
		yl_plist=intersect(plist,yl)
		###用左Y和点型交集列号从df取子数据集
		df_yl_plist=df[,c(1,yl_plist)]
		###为因子排序提前提取交集物种名
		fc_yl_plist=names(df_yl_plist)[-1]
		###子数据集变形以供画图
		df_yl_plist=melt(df_yl_plist, id.vars = names(df_yl_plist)[1])
		###因子排序
		df_yl_plist$variable=factor(df_yl_plist$variable,levels=fc_yl_plist)
		if(length(pshape)==0){
			p=p+geom_point(data=df_yl_plist, aes(x=df_yl_plist[,1], y=value, color=variable), size=lsize)
		}else{
			p=p+geom_point(data=df_yl_plist, aes(x=df_yl_plist[,1], y=value, color=variable, shape=variable), size=lsize) 
		}
	}

	##if point in right
	if(length(intersect(plist,yr))!=0){
		###右Y和线型交集列号
		yr_plist=intersect(plist,yr)
		###用右Y和线型交集列号从df取子数据集
		df_yr_plist=df[,c(1,yr_plist)]
		###为因子排序提前提取交集物种名
		fc_yr_plist=names(df_yr_plist)[-1]
		###子数据集变形以供画图
		df_yr_plist=melt(df_yr_plist, id.vars = names(df_yr_plist)[1])
		###因子排序
		df_yr_plist$variable=factor(df_yr_plist$variable,levels=fc_yr_plist)
		if(length(pshape)==0){
			p=p+geom_point(data=df_yr_plist, aes(x=df_yr_plist[,1], y=value*ryl/ryr, color=variable), size=lsize)
		}else{
			p=p+geom_point(data=df_yr_plist, aes(x=df_yr_plist[,1], y=value*ryl/ryr, color=variable, shape=variable), size=lsize)  
		}
	}

	#if(!exists("yl_plist")){yl_plist=NULL}
	#if(!exists("yr_plist")){yr_plist=NULL}

	##限制legend
	if(length(plab)==0){plab=names(df)[plist]}
	labs=setNames(plab,names(df)[plist])

	##判断点型需求
	if(length(pshape)!=0){
	p = p + scale_shape_manual(values=pshape, labels=labs) 
	}

	##判断颜色需求

	if(length(pcc)==0){
		p = p + scale_color_discrete(labels=labs)
	}else{
		cols=setNames(pcc,names(df[,plist]))	
		p = p + scale_color_manual(values=cols, labels=labs)#, limits = lgorder)
	} 

	p

	#调节副轴范围#################################
	if(length(yr)!=0){
		p = p + scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~.*ryr/ryl, name = yrlab))
	}else{
		p = p + scale_y_continuous(expand = c(0, 0))
	}

	#加yl标题#################################
	if(length(yllab)!=0){
		p = p + ylab(yllab)
	}

	#加x标题#################################
	if(length(xlab)!=0){
		p = p + xlab(xlab)
	}


	#时间列格式#################################
	p=p+theme_bw()+scale_x_datetime(date_labels="%m/%d", expand = c(0, 0))

	#调整图层顺序
	#p$layers[c(1,2,3,4,5,6)]=p$layers[c(5,6,1,2,3,4)]

	p=p+theme(legend.margin=margin(t = -0.25, r = 0.25, b = -0.25, l = 0.25, unit = "cm"),legend.title = element_blank(),legend.background = element_rect(fill=alpha('blue', 0)))

	return(p)
}