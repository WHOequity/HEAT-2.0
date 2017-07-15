
plotSummaryBar_explore_hc <- function(plotData, confInt = FALSE, ...) {

  #input$main_title2
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  
  
  #     longnames <- input$long_names2
  # indic_title_var <- "indic_name"
  # if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
  #   indic_title_var <- "indic"
  # }
  cols <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")
  
  maintitle_val <-  .rdata[["plotSummary_explore_title"]]
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  titleX <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
  titleY <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                   input$yaxis_title2,measureName)
  logY <- as.logical(plotData$logscale[1])
  plotData <- plotData %>% 
    mutate(year = as.factor(year),
           yearn = as.numeric(year) - 1,
           color = colorize(indic_name, cols[1:length(unique(indic_name))]))
  
  axislims <- c(min(plotData$yearn), max(plotData$yearn))
  plotData <- count(plotData, indic_name) %>% inner_join(plotData, ., by = "indic_name")
  
  plotDataChart <- plotData %>% 
    group_by(indic_name, dimension, indic_title) %>% 
    do(chart = {
      d <- .
      catgs <- getCats(d$year)
      hc <- highchart() %>%
        hc_xAxis(type = "category", reversed = FALSE, categories = catgs,min = axislims[1], 
                 max = axislims[2]) %>%
        hc_add_series(data = list_parse(select(d, x = yearn, y = inequal, country,measure_name = measure_name, 
                                               source, year, indic_name, dimension, 
                                               selower = se.lowerci, seupper =  se.upperci)), 
                      type = "column", 
                      threshold = ifelse(logY, 1, 0),
                      color = unique(d$color),
                      pointWidth = barWidth(d$n[1])) %>% 

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
      
      if(confInt){
              hc <- hc %>% hc_add_series(data = list_parse(select(d, x = yearn, low = se.lowerci, high = se.upperci)),
                      type = "errorbar", color = "#606060", enableMouseTracking = FALSE) 
      }
      
  hc
    })
  
  
plotDataChart$dimension2 <- plotDataChart$dimension


# TODO, fix this function so it doesn't create extra fields
plotDataChart <- minmaxAllPly(plotDataChart, plotData %>% mutate(value = inequal), 
                              confInt = confInt, logscale = logY, 
                              forceMinMax = TRUE)
# plotDataChart <- rename(plotDataChart, indic_title = indic_title.x,
#                         dimension2 = dimension2.x,
#                         ncountry = ncountry.x)


  getGrid(plotDataChart, title = maintitle_val, 
          logY = logY, minY = axismin, maxY = axismax, titleX = titleX, 
          titleY = titleY, plot_type = "plotSummaryBar_explore_hc", ...)
  
}




