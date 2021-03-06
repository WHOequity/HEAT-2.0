
plotDisagLine_explore_hc <- function(plotData, confInt = FALSE, ...) {
  #plotData <- filter(plotData, !(dimension=="Economic status" & indic == "asfr1"))
  #if(input$main_title1 == "Health Equity Disaggregated") return()
  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  #maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  plotData <- arrange(plotData, dimension, order)
  #input$main_title1
  
  isolate({
    axismin <- isolate(input$axis_limitsmin1)
    axismax <- isolate(input$axis_limitsmax1)
    titleX <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
    titleY <- ifelse(!is.null(input$yaxis_title1) && input$yaxis_title1 !="", input$yaxis_title1, "") 
  })

  
  
  
  
  # longnames <- input$long_names1
  # indic_title_var <- "indic_name"
  # if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
  #   indic_title_var <- "indic"
  # }
  
  
  plotData <- plotData %>% 
    mutate(year = as.factor(year),
           yearn = as.numeric(year) - 1)
  
  axislims <- c(min(plotData$yearn), max(plotData$yearn))
  
  plotDataChart <- plotData %>% 
    group_by(indic_name, dimension, indic_title) %>% 
    
    # for each strata this do creates a chart and adds it as 
    # list element to the resulting table.
    do(chart = {
      d <- .
      
      catgs <- getCats(d$year)
      #if(length(unique(plotData$year)) == 1)
      
      
      hc <- highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(type = "category", reversed = FALSE, categories = catgs,min = axislims[1], 
                 max = axislims[2]) %>%
        hc_tooltip(headerFormat = '', 
                   pointFormatter = JS("function(){

//if(this.indic_name == undefined){return false}
var tool = '<span class = \"tooltip-bold-bigger\">Estimate: ' + this.estimate + '</span><br>' + 
'95%CI: ' + this.lower_95ci +  '-' + this.upper_95ci +'<br><br>' +
this.country + ', ' + this.source + ' '  + this.year + '<br>' +
'<em>' + this.indic_name + '<br>' +
'<span class = \"tooltip-bold\">' + this.subgroup + ' (' + this.popshare + '% of affected population)</em></span>'; 
                              return tool;               
                              }")
        )
      
      
      # If we have more than 7 subgroups
      cnt <- length(unique(d$subgroup))
      
      if((.rdata[['HEATversion']] == "whodata" & d$dimension[1] == "Subnational region") | 
         (.rdata[['HEATversion']] == "upload" & cnt>7)) {

        d2 <- d %>%
          mutate(x = yearn, y = estimate, color = colors,
                 low = lower_95ci, high = upper_95ci) 
        
        hc <- hc %>% hc_add_series(data = list_parse(select(d2, x, y, country, 
                                                            source, year, indic_name, dimension, subgroup, 
                                                            popshare, estimate, lower_95ci, upper_95ci)),
                                   name = d2$dimension[1], type = "scatter", 
                                   color = hex_to_rgba(unique(d2$color), alpha = 0.5))
        
            d3 <- d %>%
          group_by(x = yearn) %>%
          summarize(low = min(estimate), high = max(estimate))
        
        hc <- hc %>%
          hc_add_series(data = NULL, color = "transparent", 
                        type = "line", showInLegend = FALSE) %>%
          hc_add_series(data = list_parse(d3), type = "errorbar", zIndex = -10, name = "range",
                        stemWidth = 1, whiskerLength = 1, color = "#606060", linkedTo = NULL,
                        showInLegend = FALSE, enableMouseTracking = FALSE)
      }else{
        
        
        # loop through the subgroups and for each you add a series
        # in this case the series is lines
        for(sg in unique(d$subgroup)){ # sg <- "01 dki jakarta"
          
          d2 <- d %>%
            filter(subgroup == sg) %>%
            mutate(x = yearn, y = estimate, color = colors,
                   low = lower_95ci, high = upper_95ci) 
          
          # the color should be transparent if it's a dimension with
          # more than 7 subgroups (like Subnational region). Careful
          # if you change hex_to_rgba you'll need to change getLegend
          
          
          hc <- hc %>% hc_add_series(data = list_parse(select(d2, x, y, country, 
                                                              source, year, indic_name, dimension, subgroup, 
                                                              popshare, estimate, lower_95ci, upper_95ci)),
                                     name = sg, type = "scatter", color = unique(d2$color))
        }
        
        # this gets the min and max estimates so that you can add the
        # line through
        d3 <- d %>%
          group_by(x = yearn) %>%
          summarize(low = min(estimate), high = max(estimate))
        
        hc <- hc %>%
          hc_add_series(data = NULL, color = "transparent", 
                        type = "line", showInLegend = FALSE) %>%
          hc_add_series(data = list_parse(d3), type = "errorbar", zIndex = -10, name = "range",
                        stemWidth = 1, whiskerLength = 1, color = "#606060", linkedTo = NULL,
                        showInLegend = FALSE, enableMouseTracking = FALSE)
      }
      hc
    })
  
  
  plotData <- plotData %>% mutate(value = estimate)
  plotDataChart <- minmaxAllPly(plotDataChart, plotData)

  getGrid(plotDataChart, title = maintitle_val,
          minY = axismin, maxY = axismax, titleX = titleX, titleY = titleY,
          plot_type = "plotDisagLine_explore_hc", ...)
  
}

