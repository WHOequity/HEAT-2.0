plotSummaryLine_explore_hc <- function(plotData, confInt = FALSE, ...) {
  
  #input$main_title2
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  maintitle_val <-  .rdata[["plotSummary_explore_title"]]
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  titleX <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
  titleY <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                   input$yaxis_title2, measureName)
  
  longnames <- isolate(input$long_names2)
  
  cols <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")
  
  logY <- as.logical(plotData$logscale[1])
  plotData <- plotData %>% 
    mutate(year = year,
           yearn = year,
           color = colorize(indic_name, cols[1:length(unique(indic_name))]))
  
    axislims <- c(min(plotData$yearn), max(plotData$yearn))
  
 
  tickarray  <- paste("[", paste(sort(unique(plotData$year)), collapse = ","), "]")
  ticks <- sprintf("function(){var ticks = %s; return ticks;}", tickarray)


  plotDataChart <- plotData %>% 
    group_by(aux = "", dimension) %>% 
    do(chart = {
      d <- .
      catgs <- getCats(d$year)
      hc <- highchart() %>%
        hc_xAxis(type = "category", reversed = FALSE, min = axislims[1], 
                 max = axislims[2], tickmarkPlacement = "on", tickPositioner = JS(ticks))%>%
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(headerFormat = '',
                   pointFormatter = JS("function(){
    var tool = '<span class = \"tooltip-bold-bigger\">' + this.measure_name + ': ' + this.y + '</span><br>' + 
    '95%CI: ' + this.selower +  '-' + this.seupper +'<br><br>' +
     this.country + ', ' + this.source + ' '  + this.year + '<br>' +
    '<em>' + this.indic_name + '<br>' +
    'By ' + this.dimension.toLowerCase() + '</em>'; 
     return tool;
                              }")
        )
      
      for(ind in unique(d$indic_name)){
        d2 <- filter(d, indic_name == ind)
        
        indicname <- ifelse(longnames, ind, d$indic)
        
        hc <- hc %>% 
          hc_add_series(data = list_parse(select(d2, x = yearn, y = inequal, country,
                                                 measure_name = measure_name, 
                                                 source, year, indic_name, dimension, 
                                                 selower = se.lowerci, seupper =  se.upperci)),
                        type = "line", 
                        color = unique(d2$color), 
                        name  = indicname, 
                        id = ind)
        
        if(input$summary_error_bars){
          hc <- hc %>%  hc_add_series(data = list_parse(select(d2, x = yearn, 
                                         low = se.lowerci, high = se.upperci)),
                                      type = "errorbar", color = unique(d2$color), 
                                      linkedTo = ind, 
                                      enableMouseTracking = FALSE)
        }
        
      }
      
      hc
      
      
    })
  
  # change flip
  
  plotDataChart$dimension2 <- plotDataChart$dimension
  plotDataChart <- plotDataChart %>%
    rename(indic_name = dimension,
           dimension = aux)
  
    plotData <- plotData %>% mutate(value = inequal)
  plotDataChart <- minmaxAllPly(plotDataChart, plotData, confInt = confInt, logscale = logY)
  plotDataChart$indic_title <- plotDataChart$indic_name
  
  getGrid(plotDataChart, title = maintitle_val, logY = logY, 
          minY = axismin, maxY = axismax, titleX = titleX, titleY = titleY, 
          plot_type = "plotSummaryLine_explore_hc", ...) 
  
}


