
plotSummaryScatter_compare_hc <- function(plotData, ...) {
  
  #input$main_title4
  
  plotStyle <- isolate(input$disag_plot_summary_pts)
  maintitle_val <- .rdata[['plotSummary_compare_title']]
  Xaxismin <- isolate(input$xaxis_limitsmin4)
  Xaxismax <- isolate(input$xaxis_limitsmax4)
  Yaxismin <- isolate(input$yaxis_limitsmin4)
  Yaxismax <- isolate(input$yaxis_limitsmax4)

  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  ncountry <- length(unique(plotData$country))
  dimension2 <- unique(plotData$dimension)
  indic_name2 <- unique(plotData$indic_name)
  
  titleX <- ifelse(!is.null(isolate(input$xaxis_title4)) && isolate(input$xaxis_title4) !="", isolate(input$xaxis_title4), "National average")
  titleY <- ifelse(!is.null(isolate(input$yaxis_title4)) && isolate(input$yaxis_title4) !="", 
                   input$yaxis_title4, measureName)
  
  logY <- as.logical(plotData$logscale[1])
  
  if(sum(plotData$anchor==0) == 0){
    # this dummy is added with NA data except for the benchmark
    # so that the legend and color-coding works in the plot
    anchordummy <- plotData[1,]
    anchordummy[, names(anchordummy)!="anchor"]  <- NA
    anchordummy$anchor <- 0
    plotData <- rbind(plotData, anchordummy)
  }
  #"#235ba3", "#b54848"
  plotData$color <- factor(plotData$anchor, levels = c(0,1), labels = c("#235ba3", "#b54848"))
  plotData <- plotData[order(plotData$anchor), ] %>% 
    mutate(anchor_label = ifelse(anchor, country, "Benchmark countries")) 
  
  plotData <- filter(plotData, !is.na(estimate) | !is.na(inequal)) 
  if(nrow(plotData)){
    plotData$inequal <- round(plotData$inequal,2)
    plotData$estimate <- round(plotData$estimate,2)
  }

  plotData <- plotData %>% 
    group_by(anchor_label) %>% 
    do(serie = {
      d <- .
      lst <- list(data = list_parse(select(d, x = estimate,  y = inequal, indic_title, 
                                    country, source, year, indic_name, measure_name, dimension, ccode)),
           type = "scatter", name = unique(d$anchor_label), color = as.character(unique(.$color)),
           animation = FALSE
           )
      
      
      if(plotStyle == "labels"){
        lst[['dataLabels']] <- list(enabled = TRUE, formatter = JS('function(){return this.point.ccode}'),
                                             style = list(color = unique(.$color)),
                                             align = "center",
                                             verticalAlign = "middle")
        
        lst$color<- paste("rgba(", paste(col2rgb(lst$color), collapse=","), ",0.0)")
      }
      
      
      lst
    })
  
  chart <- highchart() %>% 
    hc_plotOptions(
    series = list(
      marker = list(
        radius = 6,
        fillOpacity = 0.2
      )
    )
  ) %>%
    hc_add_series_list(plotData$serie) %>% 
    hc_tooltip(headerFormat = '', 
               pointFormatter = JS("function(){


    var tool = '<span class = \"tooltip-bold-bigger\">National average: ' + this.x + '</span><br>' + 
    this.measure_name + ': ' + this.y + '<br><br>' +
     this.country + ', ' + this.source + ' '  + this.year + '<br>' +
     '<em>' + this.indic_name + '<br>' +
     'By ' + this.dimension.toLowerCase() + '</em>'; 
                                   return tool;
                              }")
    )
  

  plotDataChart <- data_frame(
    indic_name = "",
    dimension = "",
    dimension2 = dimension2,
    indic_name2 = indic_name2,
    ncountry = ncountry,
    chart = list(chart))
  
  
  
  
  getGrid(plotDataChart, title = maintitle_val, logY = logY, 
          minY = Yaxismin, maxY = Yaxismax, plot_type = "plotSummaryScatter_compare_hc",
          minX = Xaxismin, maxX = Xaxismax, 
          indic_title = FALSE, titleX = titleX, titleY = titleY,...)
  
}


