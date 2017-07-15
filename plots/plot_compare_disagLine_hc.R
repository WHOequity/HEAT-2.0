
plotDisagLine_compare_hc <- function(plotData, ...) {
  #browser()  
  
  #if(input$main_title3 == "Health Equity Summary") return()
  maintitle_val <- .rdata[['plotDisag_compare_title']]
  axismin <- isolate(input$axis_limitsmin3)
  axismax <- isolate(input$axis_limitsmax3)
  
  titleX <- ifelse(!is.null(isolate(input$xaxis_title3)) && 
                     isolate(input$xaxis_title3) !="", isolate(input$xaxis_title3), "")
  titleY <- ifelse(!is.null(isolate(input$yaxis_title3)) && 
                     isolate(input$yaxis_title3) !="", isolate(input$yaxis_title3), "")

  ncountry <- length(unique(plotData$country))
  plotDataChart <- plotData %>% 
    mutate(country_orig = country,
           country = paste0(country, " (", source, " ", year, ")")) %>% 
    dplyr::arrange(desc(anchor), country) %>%
    ungroup() %>% 
    mutate(country = factor(country, levels=unique(country))) %>% 
    group_by(indic_name, indic_title) %>% 
    do(chart = {
      d <- .
      .
      hc <- highchart() %>% 
        hc_xAxis(type = "category", reversed = TRUE) %>% 
        hc_chart(type = "bar") %>% 
        # rotate tooltip # 19
        hc_tooltip(headerFormat = '',
                   pointFormatter = JS("function(){


         //if(this.indic_name == undefined){return false}
         var tool = '<span class = \"tooltip-bold-bigger\">Estimate: ' + this.y + '</span><br>' + 
         '95%CI: ' + this.lower_95ci +  '-' + this.upper_95ci +'<br><br>' +
         this.country_orig + ', ' + this.source + ' '  + this.year + '<br>' +
         '<em>' + this.indic_name + '<br>' +
         '<span class = \"tooltip-bold\">' + this.subgroup + ' (' + this.popshare + '% of affected population)</em></span>'; 
         return tool; 
                              }")
        )
      
      
      
      cnt <- length(unique(d$subgroup))
      
      if((.rdata[['HEATversion']] == "whodata" & d$dimension[1] == "Subnational region") | 
         (.rdata[['HEATversion']] == "upload" & cnt>7)){
        
                  d2 <- d %>%
            mutate(name = country, y = estimate, color = colors) 
          
          hc <- hc %>% hc_add_series(data = list_parse(select(d2, name, y, country_orig, indic_title,
                                                              source, year, indic_name, dimension, subgroup, 
                                                              popshare,  lower_95ci, upper_95ci)), 
                                     name = d2$dimension[1],
                                     type = "scatter", 
                                     color = hex_to_rgba(unique(d2$color), alpha = 0.5))
        
      } else {
        
        for(sg in unique(d$subgroup)){
          d2 <- d %>%
            filter(subgroup == sg) %>%
            mutate(name = country, y = estimate, color = colors) 
          
          hc <- hc %>% hc_add_series(data = list_parse(select(d2, name, y, country_orig, indic_title,
                                                              source, year, indic_name, dimension, subgroup, 
                                                              popshare,  lower_95ci, upper_95ci)), name = sg,
                                     type = "scatter", color = unique(d2$color))
        }
        
        
      }
      
      
      
      
      
      
      
      
      d3 <- d %>% 
        group_by(name = country) %>% 
        summarize(low = min(estimate), high = max(estimate))
      
      hc <- hc %>% 
        hc_add_series(data = NULL, color = "transparent", type = "line", showInLegend = FALSE) %>%
        hc_add_series(data = list_parse(d3), type = "errorbar", zIndex = -10, name = "range",
                      showInLegend = FALSE, enableMouseTracking = FALSE, linkedTo = NULL,
                      stemWidth = 1, whiskerLength = 1, color = "#606060")
      
      hc
      
      
    })
  
  
  #plotDataChart$dimension2 <- unique(plotData$dimension)
  plotDataChart <- plotDataChart %>% mutate(dimension = "",
                                            dimension2 = unique(plotData$dimension))
  plotDataChart$ncountry <- ncountry
  
  plotDataChart <- maxIndPly(plotDataChart, plotData %>% mutate(value = estimate))


  # plotDataChart <- rename(plotDataChart, indic_title = indic_title.x,
  #                       dimension2 = dimension2.x)
  
  getGrid(plotDataChart, title = maintitle_val, indic_title = FALSE, 
          minY = axismin, maxY = axismax, titleX = titleX, titleY = titleY,
          plot_type = "plotDisagLine_compare_hc", ...)
  
}



