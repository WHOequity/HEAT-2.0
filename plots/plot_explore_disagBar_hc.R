
plotDisagBar_explore_hc <- function(plotData, confInt = FALSE, ...) {

  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismax <- isolate(input$axis_limitsmax1)
  
  titleX <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
  titleY <- ifelse(!is.null(input$yaxis_title1) && input$yaxis_title1 !="", input$yaxis_title1, "")
  
  ERR_BARS <- FALSE # sample(c(TRUE, FALSE), size = 1)
  TYPE <- "column" #sample(c("column", "line"), size = 1, prob = c(0.9, .1))
  
  

  
  
  plotData <- plotData %>% 
    mutate(year = as.factor(year),
           yearn = as.numeric(year) - 1)
  
  
   axislims <- c(min(plotData$yearn), max(plotData$yearn))


  plotData <- count(plotData, indic_name) %>% inner_join(plotData, ., by = "indic_name")
  #plotData <- arrange(plotData, desc(estimate)) %>% group_by(year, indic_name) %>% mutate(rank = row_number())
  
  plotDataChart <- plotData %>% 
    group_by(indic_name, dimension, indic_title) %>% 
    do(chart = {
      d <- .
      catgs <- getCats(d$year)
      hc <- highchart() %>%
        hc_xAxis(type = "category", reversed = FALSE, 
                 categories = catgs,
                 min = axislims[1], 
                 max = axislims[2]) %>%
        hc_legend(enabled = FALSE) %>% 
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
      
      cnt <- length(unique(d$subgroup))
      if(cnt>7){
        d <- arrange(d, estimate) %>% group_by(year, indic_name) %>% mutate(rank = row_number())
        for(sg in unique(d$rank)){
          # sg <- d$subgroup[1]
          d2 <- d %>%
            filter(rank == sg) %>%
            mutate(x = yearn, y = estimate, color = colors,
                   low = lower_95ci, high = upper_95ci)
          
          
          
          
          hc <- hc %>% 
            hc_add_series(data = list_parse(select(d2, x, y, country, 
                                                   source, year, indic_name, dimension, subgroup, 
                                                   popshare, estimate, lower_95ci, upper_95ci)), 
                          name = d$dimension[1], type = TYPE, color = unique(d2$color),
                          pointPadding = 0.05)
          
          if(confInt) {
            hc <- hc %>%
              hc_add_series(data = list_parse(select(d2, x, low, high)),
                            type = "errorbar", color = "#606060", enableMouseTracking = FALSE)
          }
          
        }
      }else{
        for(sg in unique(d$subgroup)){
          # sg <- d$subgroup[1]
          d2 <- d %>%
            filter(subgroup == sg) %>%
            mutate(x = yearn, y = estimate, color = colors,
                   low = lower_95ci, high = upper_95ci)
          
          
          
      
          hc <- hc %>% 
            hc_add_series(data = list_parse(select(d2, x, y, country, 
                                                   source, year, indic_name, dimension, subgroup, 
                                                   popshare, estimate, lower_95ci, upper_95ci)), 
                          name = sg, type = TYPE, color = unique(d2$color),
                          pointWidth = barWidth(d2$n[1]))
          
          if(input$disag_error_bars) {
            hc <- hc %>%
              hc_add_series(data = list_parse(select(d2, x, low, high)),
                            type = "errorbar", color = "#606060", enableMouseTracking = FALSE)
          }
          
        }
    }
      #}
      hc
    })
  
  #browser()
  plotDataChart <- maxIndPly(plotDataChart, plotData %>% mutate(value = estimate), confInt = confInt)
  #plotDataChart <- rename(plotDataChart, indic_title = indic_title.x)

  # Dont' apply justification for those with more than 7 subgroups
  cnts <- count(distinct(plotData, country,  dimension, subgroup), country, dimension)
  cnts <- which(!plotDataChart$dimension%in%cnts$dimension[cnts$n>7])
  
  #plotDataChart$chart[cnts] <- map(plotDataChart$chart[cnts], addJustify) # 57

  getGrid(plotDataChart, title = maintitle_val, 
          minY = axismin, maxY = axismax, 
          titleX = titleX, 
          titleY = titleY,
          indic_title_var = indic_title_var,
          plot_type = "plotDisagBar_explore_hc",...)
  
}


