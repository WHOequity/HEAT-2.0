# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

####PLOT 4
plotSummaryLine_explore_ggplot <- function(plotData, session){
  #  Plot 4: Line chart for a single country (Disaggregation of data)
  

  
  #input$main_title2
  sumMeasure <- plotData$measure[1]
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  yrs <-paste(sort(unique(plotData$year)), collapse=", ")
  sources <- paste(sort(unique(plotData$source)), collapse=" & ")
  
  isolate({
    xtitle_val <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
    ytitle_val <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                         input$yaxis_title2,  measureName)
  })

  
  maintitle_val <-  .rdata[["plotSummary_explore_title"]]
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  
  
  
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  
  
  plotData$year <- as.integer(plotData$year)
  
  
  
  numyears<-length(unique(plotData$year))
  errwidth <- ifelse(numyears>2, 0.5, 0.25)
  
  if(numyears==1){
    
    tmp1 <- plotData
    tmp1$year <- tmp1$year+1
    tmp1$inequal <- NA
    tmp1$se.lowerci<-NA
    tmp1$se.upperci<-NA

    tmp2 <- plotData
    tmp2$year <- tmp1$year-2
    tmp2$inequal <- NA
    tmp2$se.lowerci<-NA
    tmp2$se.upperci<-NA


    plotData<-rbind(plotData, tmp1, tmp2)
    
    
  }
  

  
  ci_type <-  c("se.lowerci", "se.upperci")
  # ifelse(is.null(input$summary_CI_type) || input$summary_CI_type == "analytic", 
  #        ci_type <-  c("se.lowerci", "se.upperci"), 
  #        ci_type <-  c("boot.lowerci", "boot.upperci"))
  
  
  indictype <- "indic_name"
  if(!is.null(input$long_names2) && input$long_names2 == FALSE) indictype <- "indic"
  form <- paste(indictype, " ~ dimension")
  #indictype <- "indic_name"
  p<-ggplot(plotData, aes_string(x="year", y="inequal", group=indictype, color=indictype))+
    geom_line(size=1) +
    geom_point(size=4)+
    geom_errorbar(aes_string(ymin=ci_type[1], ymax=ci_type[2]),   width=errwidth, alpha=input$summary_error_bars)+
    labs(x=xtitle_val, y=ytitle_val, title=maintitle_val)+
    scale_x_continuous(breaks=sort(unique(plotData$year)))+
    guides(col=guide_legend(ncol=1)) +
    heat_theme() +  
    #coord_cartesian(xlim=c(as.numeric(input$axis_limitsmin2), as.numeric(input$axis_limitsmax2)))+
    #ylim(as.numeric(input$axis_limitsmin2),  as.numeric(input$axis_limitsmax2))+
    facet_grid(dimension~.)
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }

  if(logscale==1){
    
    if(any(c(axismin, axismax)<=0)) p <- p + coord_cartesian()
    brk <- unique(c(1,round(pretty(c(plotData$se.lowerci, plotData$se.upperci), n=5))))
    brk <- brk[brk>0]
    p <- p + geom_hline(yintercept=1, alpha=0)+scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", ytitle_val))
    
    
  }
  
  
  
  
  return(p)
}
