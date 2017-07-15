
#### PLOT 3
plotSummaryBar_explore_ggplot <- function(plotData, session){
  # Plot 3: Barchart for a single country (Summary data)
  
  
  
  sumMeasure <- plotData$measure[1]
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  yrs <-paste(sort(unique(plotData$year)), collapse=", ")
  sources <- paste(sort(unique(plotData$source)), collapse=" & ")
  
  
  isolate({
    #input$main_title2
    xtitle_val <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
    ytitle_val <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                         input$yaxis_title2,  measureName)
  })
  
  
  maintitle_val <- .rdata[["plotSummary_explore_title"]]
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  numyears <- length(unique(plotData$year))
  
  
  binwidth <- ifelse(numyears>2, 0.75, 0.35)
  errwidth <- ifelse(numyears>2, 0.5, 0.25)
  
  
  #print("plotFigure3() in plotter.R")
  # Make sure that the data frame includes all the possible bars by adding missing data across the factor levels
  plotData <- plotData[,c('year', 'indic_name','indic', "dimension",'inequal', 
                          'se.lowerci', 'se.upperci')]
  
  
  #probably I need this but not clear why
  allposs <-  expand.grid (year = unique(plotData$year), 
                           indic = unique(plotData$indic),
                           indic_name = unique(plotData$indic_name),
                           dimension = unique(plotData$dimension), stringsAsFactors = FALSE)
  
  plotData <- left_join(allposs, plotData, by=c("year", "indic", "indic_name", "dimension"))
  plotData$year <- as.factor(plotData$year)
  
  
  indictype <- "indic_name"
  if(!is.null(input$long_names2) && input$long_names2 == FALSE) indictype <- "indic"
  form <- paste(indictype, " ~ dimension")
  
  
  
  ci_type <-  c("se.lowerci", "se.upperci") 
  
  
  
  p <- ggplot(plotData, aes_string(x = "year", weight = "inequal", 
                                   fill=indictype,ymin=ci_type[1], ymax=ci_type[2]))+ 
    geom_text(data=plotData, aes_string(x = "year", y="inequal",fill=indictype, 
                                        label="format(round(inequal,1),nsmall=1)",ymin=ci_type[1], ymax=ci_type[2]), 
              position=position_dodge(width=binwidth), vjust=-0.75,
              size=3, color="grey50", alpha=!input$summary_error_bars)+
    geom_bar(aes(y=inequal), position=position_dodge(), stat="identity", color='white', 
             size=1, width=binwidth)+
    geom_errorbar(color="grey50",  
                  position=position_dodge(0.9), width=0.25, alpha=input$summary_error_bars)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    guides(colour=FALSE, fill=FALSE)+ 
    heat_theme() + 
    facet_grid(form, labeller = splitLabels)
  
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }
  
  if(logscale==1){
    if(any(c(axismin, axismax)<=0)) p <- p + coord_cartesian()
    brk <- c(1,unique(round(pretty(c(plotData$se.lowerci, plotData$se.upperci), n=5))))
    p <- p + scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", ytitle_val))
  }
  
  
  return(p)
}
