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

plotSummaryScatter_compare_ggplot <- function(plotData, session){
  #  Plot 6: Scatter plot showing benchmark countries National Average against inequality
  


  
  sumMeasure <- plotData$measure[1]
  
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  isolate({
    #input$main_title4
    ptsVSlabels <- input$disag_plot_summary_pts
    titleX <- ifelse(!is.null(isolate(input$xaxis_title4)) && isolate(input$xaxis_title4) !="", isolate(input$xaxis_title4), "Setting average")
    titleY <- ifelse(!is.null(isolate(input$yaxis_title4)) && isolate(input$yaxis_title4) !="", 
                     input$yaxis_title4,measureName)
  })

  
  Xaxismin <- isolate(input$xaxis_limitsmin4)
  Xaxismax <- isolate(input$xaxis_limitsmax4)
  Yaxismin <- isolate(input$yaxis_limitsmin4)
  Yaxismax <- isolate(input$yaxis_limitsmax4)
  
  
  if(sum(plotData$anchor==0) == 0){
       # this dummy is added with NA data except for the benchmark
    # so that the legend and color-coding works in the plot
    
    anchordummy <- plotData[1,]
    anchordummy[, names(anchordummy)!="anchor"]  <- NA
    anchordummy$anchor <- 0
    plotData <- rbind(plotData, anchordummy)
  }
  
  
  
  plotPch <- c(19, 15)
  plotPalette <- c('#00616B', '#6B0A00')
  
  plotData <- plotData[order(plotData$anchor), ]
  
  maintitle_val <- .rdata[['plotSummary_compare_title']]
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  p <- ggplot(plotData, aes(x=estimate, y=inequal), color=plotPalette)+
    labs(x=titleX, y=titleY, title=maintitle_val)
  
 
  if(ptsVSlabels == "points"){  # Plot dots, or Country codes
    p <- p + geom_point(aes(color=as.factor(anchor), shape=as.factor(anchor)), size=4)
    
    
    
  } else {
    
    p <- p + geom_text(aes(label=ccode, color=as.factor(anchor)), 
                       hjust=0.5, vjust=0.5, size=3.5, show_guide=FALSE)+
      geom_line(aes(color=as.factor(anchor)), size=0, alpha=0)+
      guides(colour = guide_legend(override.aes = list(size=2, alpha=1)))+heat_theme()
    
    
    
  }
  
  p <- p + scale_shape_manual(name  ="",
                              breaks=c(0, 1),
                              labels=c("Benchmark settings", plotData$country[plotData$anchor==1][1]),
                              values=plotPch)
  
  p <- p + scale_colour_manual(name  ="",
                               breaks=c(0, 1),
                               labels=c("Benchmark settings", plotData$country[plotData$anchor==1][1]),
                               values=plotPalette)  
  
  
  
  # if the user makes changes to any of the axes

  if(any(c(Xaxismin, Xaxismax, Yaxismin, Yaxismax)!="")) {
    
    pbuild<-ggplot_build(p)
    xaxisrange<-pbuild$layout$panel_ranges[[1]]$x.range
    Xaxismin<-ifelse(Xaxismin=="", xaxisrange[1], as.numeric(Xaxismin))
    Xaxismax<-ifelse(Xaxismax=="", xaxisrange[2], as.numeric(Xaxismax))
    yaxisrange<-pbuild$layout$panel_ranges[[1]]$y.range
    Yaxismin<-ifelse(Yaxismin=="", yaxisrange[1], as.numeric(Yaxismin))
    Yaxismax<-ifelse(Yaxismax=="", yaxisrange[2], as.numeric(Yaxismax))
    
    
    p <- p + coord_cartesian(xlim=c(Xaxismin, Xaxismax), ylim=c(Yaxismin, Yaxismax))

  }

  
  p<-p+heat_theme()
  
  if(logscale==1){
    
    if(any(c(Yaxismin, Yaxismax)<=0)) p <- p + coord_cartesian()
    brk <- sort(unique(c(1,unique(round(pretty(c(plotData$inequal), n=5))))))
    brk <- brk[brk!=0]
    p <- p + geom_hline(yintercept=1, alpha=0) + scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", titleY))
  }
  
  
  
  p
}


