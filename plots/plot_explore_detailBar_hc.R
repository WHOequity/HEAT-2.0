
# function ----------------------------------------------------------------
plotDetailBar_explore_hc <- function(plotData, sortBy = NULL, regs = NULL, 
                                     showAVG = TRUE, showMedian = TRUE, 
                                     addGroupNames = TRUE, indicSort = NULL, ...) {


  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  isolate({
    disag_plot_explore_dtl_sort      <- input$disag_plot_explore_dtl_sort
    disag_plot_explore_dtl_showAVG   <- input$disag_plot_explore_dtl_showAVG
    disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
    descend <- input$sortOrder_ind_dim == "Descending"
    disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
    disag_plot_explore_dtl_showNames <- input$disag_plot_explore_dtl_showNames
    disag_indicSort <- input$focus_indicator_explore_plotdtl[input$focus_indicator_explore_plotdtl%in%unique(plotData$indic)]
  })
  
  if(.rdata[['is_modal']]){
    sortBy  <- disag_plot_explore_dtl_sort
    regs    <- disag_plot_explore_dtl_subgroups
    showAvg <- disag_plot_explore_dtl_showAVG
    showMedian <- disag_plot_explore_dtl_showMedian
    addGroupNames <- disag_plot_explore_dtl_showNames
    indicSort <- disag_indicSort
  }

  #git 717
  if(is.null(regs)) regs <- "None"

  dfindic <- distinct(plotData, indic, indic_name, subgroup, source, national,
                      lower_95ci, upper_95ci, popshare, country, year) 

  plotData <- plotData %>% 
    select(indic, dimension, subgroup, estimate, colors) %>% 
    mutate(indic = factor(indic, levels = unique(plotData$indic))) %>% 
    spread(indic, estimate)
  
  
  
  
  if(!is.null(sortBy) && sortBy != "") {
    
    if(descend){
      plotData <- plotData %>% 
        arrange_(paste0("desc(", sortBy, ")"))
    }else{
      plotData <- plotData %>% 
        arrange_(sortBy)
    }

  }
  

  plotData <- plotData %>%
    mutate(ord = seq(1, nrow(.)))
  
 
  plotData <- plotData %>% 
    rename(color = colors) %>% 
    mutate(color = ifelse(subgroup %in% regs, "red", color)) %>%
    gather(indic, estimate, -dimension, -subgroup, -ord, -color) %>% 
    left_join(dfindic, by = c("indic",  "subgroup"))

  plotData$color <- apply(t(col2rgb(plotData$color)), 1, function(x) paste("rgba(", paste(x, collapse=","), ",0.95)"))
  plotDataChart <- plotData %>% 
    group_by(indic_name) %>% 
    do(chart = {
      d <- .
      ds <- d %>% 
        arrange(ord) %>% 
        mutate(x = ord, y = estimate, name = subgroup) #%>% 
        #select(x, y, name, color)
      hc <- highchart() %>%
        hc_chart(type = "bar") %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_add_series(data = list_parse(ds), 
                      type = "column",
                      name = unique(d$indic_name),
                      pointRange = 1,
                      groupPadding = 0,
                      pointWidth = barWidth(nrow(ds), detailedBar = TRUE),
                      pointPadding =  0,
                      borderWidth = 0
                      ) 
    

      if(addGroupNames){
        hc <- hc %>% hc_xAxis(visible = addGroupNames, 
                              categories = c("", as.character(ds$name)))
      }else{
        hc <- hc %>% hc_xAxis(visible = FALSE)
      }
        
      
      if(d$indic[1] != indicSort[1] & addGroupNames){
        hc <- hc %>% hc_xAxis(labels = list(style = list(opacity=0)))
      }
      
      
      addLines <- NULL
      
      if(showAVG) {
        
        avgLine <- list(color = "black",
                        label = list(text = paste("National average: ", unique(d$national)),
                                     style = list("text-shadow" = "2px 2px white")),
                        width = 1,
                        zIndex = 3,
                        value = unique(d$national))
        
        addLines <- append(addLines, list(avgLine))

      }
      
      
      if(showMedian) {
        
        medianLine <-    list(color = "#C66839",
                              #dashStyle = "Dash",
                              label = list(text = paste("Median: ", median(d$estimate)), 
                                           align = "left", 
                                           style = list(color = "#C66839", "text-shadow" = "2px 2px white")),
                              width = 1,
                              zIndex =3,
                              value = median(d$estimate))
        
          addLines <- append(addLines, list(medianLine))
      }
      
      
      
      if(length(addLines)>0){
        hc <- hc %>% 
          hc_yAxis(plotLines = addLines)
      }
      
      
      hc <-hc %>% 
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
      
      
      hc
      
      
    })

  plotDataChart <- maxIndPly(plotDataChart, 
                             plotData %>% mutate(value = estimate)) 
  
  plotDataChart <- plotDataChart %>% 
    rename(dimension = indic_name) %>% 
    mutate(indic_name = "",
           indic_title = unique(plotData$dimension))
  
  ind <- distinct(plotData, indic, indic_name)
  indicSort <- ind$indic_name[match(indicSort, ind$indic)]

  if(!is.null(indicSort)){
    plotDataChart <- plotDataChart[match(indicSort, plotDataChart$dimension),]
  }

  

    
    getGrid(plotDataChart, 
          title = maintitle_val, 
          #minY = axismin,
          #maxY = axismax, 
          #titleX = titleX, 
          #titleY = titleY,
          legend = FALSE,
          #indic_title_var = indic_title_var,
          plot_type = "plotDetailBar_explore_hc",
          subgroup_count = length(unique(plotData$subgrou)),...)
  
}
