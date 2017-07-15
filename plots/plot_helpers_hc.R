





options(highcharter.theme = hc_theme_smpl(
  legend = list(enabled = FALSE, align = "center"),
  title = list(style = list(fontWeight = "normal"),  align = "center"),
  yAxis = list(endOnTick = TRUE), #53
  tooltip = list(useHTML = TRUE),
  plotOptions = list(scatter = list(marker = list(symbol = "circle")),
                     line = list(marker = list(enabled = TRUE)),
                     column = list(marker = list(symbol = "square"))), #48
  subtitle = list(style = list(fontWeight = "normal"),  align = "center")
))



div <- tags$div

# # http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
# roundUp <- function(x, percent = 10, logscale = FALSE){
# 
#   if(x<0 & !(logscale & x<=1)){
#     x <- x-(x*(percent/100))
#   }
#   
#   if(x>0 & !(logscale & x<=1)){
#     x <- x+(x*(percent/100))
#   }
#   
#   if(logscale && x <= 1){
#     x <- x/2
#   }
#   
#   x
#   
# }
# 
# roundDown <- function(x, percent = 10, logscale = FALSE){
#   
#   if(x<0 & !(logscale & x<=1)){
#     x <- x+(x*(percent/100))
#   }
#   
#   if(x>0 & !(logscale & x<=1)){
#     x <- x-(x*(percent/100))
#   }
#   
# 
#   if(logscale && x <= 1){
#     x <- x/2
#   }
#   
#   x
#   
# }


newMinMax <- function(minval, maxval = NULL, thepercent = 10, logscale = FALSE){
  
  
  if(!is.null(maxval)){
    thediff <- minval - maxval
    if(thediff == 0){
      diffper <- abs(minval) * thepercent/100
    }else{
      diffper <- abs(thediff) * (thepercent/100)
    }
    
    newmin <- minval - diffper
    newmax <- maxval + diffper
  }else{
    
    newmin <- minval - (abs(minval) * thepercent/100)
    newmax <- NULL
  }
  

  c(min = newmin, max = newmax)
}



minmaxAllPly <- function(plotDataChart, plotData, confInt = FALSE, logscale = FALSE, forceMinMax = FALSE) {
#browser()
  if(!confInt){ #git772
  minmaxAll <- plotData %>% 
    summarise(maxReal = max(value, na.rm = TRUE),
              minReal = min(value, na.rm = TRUE)) 
  }else{
    # group_by(indic_name, dimension) %>% 
    minmaxAll <- plotData %>% 
    summarise(maxReal = max(c(se.lowerci, se.upperci), na.rm = TRUE),
              minReal = min(c(se.lowerci, se.upperci), na.rm = TRUE)) 
  }
  
  newvals <- newMinMax(minmaxAll$minReal, minmaxAll$maxReal)
  
  minmaxAll <- mutate(minmaxAll, minAll = unname(newvals['min']), maxAll = unname(newvals['max']))

  
  
if(forceMinMax){
  
  if(all(plotData$value>0) & !logscale) minmaxAll$minAll <- 0
  if(all(plotData$value<0) & !logscale) minmaxAll$maxAll <- 0
  if(all(plotData$value>1) & logscale) minmaxAll$minAll <- 1
  if(all(plotData$value<1) & logscale) minmaxAll$maxAll <- 1
}
  

  plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, 
                             max = unname(minmaxAll$maxAll), min =unname(minmaxAll$minAll))
  
  if(logscale){
    #plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, tickInterval = 0.2)
  }
  
  
  plotDataChart
  
}


maxIndPly <- function(plotDataChart, plotData, confInt = FALSE){
  

  if(!confInt){ #git772
    maxInd <- plotData %>%
      group_by(indic_name) %>%
      summarise(maxReal = max(value, na.rm = TRUE)) %>%
      mutate(maxIndic = maxReal)
  }else{
    maxInd <- plotData %>%
      group_by(indic_name) %>%
      summarise(maxReal = max(c(lower_95ci, upper_95ci), na.rm = TRUE)) %>%
      mutate(maxIndic = maxReal)
  }
  

  newvals <- newMinMax(maxInd$maxIndic)
  maxInd$maxInd <- newvals['max']
  #git612
  #maxInd$maxIndic[grepl("%", maxInd$indic_name) & maxInd$maxReal>90] <- 100
  
  plotDataChartAux <- left_join(plotDataChart, maxInd, by = "indic_name")
  
  plotDataChartAux$chart <- map2(plotDataChartAux$chart, 
                                 plotDataChartAux$maxIndic, function(hc, mx){
                                   hc_yAxis(hc, max = mx)
                                 })
  
  #joinFlds <- "indic_name"
  #if("dimension"%in%names(plotDataChart)) joinFlds <- c(joinFlds, "dimension")
  
  joinFlds <- names(plotDataChart)[names(plotDataChart)%in%names(plotDataChartAux)]
  joinFlds <- joinFlds[joinFlds!="chart"]
  
  plotDataChart <- plotDataChart %>% 
    select(-chart) %>% 
    left_join(plotDataChartAux, by = joinFlds) %>% 
    select(-maxIndic)
  
  plotDataChart
  
  
}





addJustify <- function(hc) {
  # http://jsfiddle.net/zmktekak/14/
  hc %>% 
    hc_chart(
      animation = FALSE,
      events = list(
        load = JS("function () {justifyColumns(this);}"),
        redraw = JS("function () {justifyColumns(this);}")
      )
    ) %>% 
    hc_xAxis(
      cosshair = TRUE
    ) %>% 
    hc_plotOptions(
      series = list(
        animation = FALSE,
        events = list(
          show = JS("function () {justifyColumns(this.chart);}"),
          hide = JS("function () {justifyColumns(this.chart);}")
        )
      )
    )
}

getCats <- function(x) {
  
  if(is.factor(x)) {
    catgs <- levels(x)
  } else {
    catgs <- as.character(sort(unique(x)))
  }
  
  if(length(catgs) == 1)
    catgs <- list(catgs)
  
  
  catgs
  
}


observeEvent(input$nchart, {
  
  if(input$who_heat == "Explore Inequality") { # is possible know that?
    toggleModal(session, "hc_model_explore", "open")
  } else {
    toggleModal(session, "hc_model_compare", "open")
  }
})



# This is the modal 
output$zoomhc_explore <- renderHighchart({
  

  .rdata[['is_modal']] <<- TRUE
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore) &&
     input$assessment_panel == "dataplot"){
    focus_data <- .rdata[["focus_plot_data_disag_explore"]]
    whichPlot <- ifelse(input$disag_plot_type_explore == "Line", "plotDisagLine_explore_hc",
                        "plotDisagBar_explore_hc")
    
  }
  
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore)
     && input$assessment_panel == "dataplot_dtl"){
    
    focus_data <- .rdata[['focus_plot_data_disag_dtl_explore']]
    whichPlot <- "plotDetailBar_explore_hc"
    
  }
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore)
     && input$assessment_panel == "sumplot"){
    
    focus_data <- .rdata[["focus_plot_data_summary_explore"]]
    whichPlot <- ifelse(input$summary_plot_type_explore == "Line", "plotSummaryLine_explore_hc",
                        "plotSummaryBar_explore_hc")
    
  }

  
  if(!is.null(input$inequaldisag)  && input$inequaldisag == "sumplot"){
    
    focus_data <- .rdata[["focus_plot_data_summary_explore"]]
    whichPlot <- ifelse(input$summary_plot_type_explore == "Line", "plotSummaryLine_explore_hc",
                        "plotSummaryBar_explore_hc")
    
  }
  
  
  
  fun_hc <- get(whichPlot)
  hc <- fun_hc(focus_data, nchart = floor(input$nchart))


  .rdata[['is_modal']] <<- FALSE
  
  hc %>%
    hc_legend(enabled = 
                !whichPlot %in% c("plotSummaryBar_explore_hc", "plotDetailBar_explore_hc") && length(unique(focus_data$subgroup))<=7) %>%
    hc_size(height = NULL)
  
  
})




output$zoomhc_compare <- renderHighchart({
  
  
  if(!is.null(input$comparison_panel)){
    
    focus_data_source <-ifelse(input$comparison_panel == "inequaldisag", 
                               "focus_plot_data_disag_compare",
                               "focus_plot_data_summary_compare")
    focus_data <- .rdata[[focus_data_source]]
    whichPlot <- ifelse(input$comparison_panel == "inequaldisag", "plotDisagLine_compare_hc",
                        "plotSummaryScatter_compare_hc")
    
    
  }
  
  
  
  fun_hc <- get(whichPlot)
  hc <- fun_hc(focus_data, nchart = floor(input$nchart))
  
 
  hc %>%
    hc_legend(enabled = !length(hc$x$hc_opts$series) > 7) %>%
    hc_size(height = NULL)
  
  
})







# width_facet_p is the space devoted to the indicator title (converted to %)
# width_yaxis_p is the space devoted to the yaxis title

getGrid <- function(plotDataChart, title = "This is the main title", legend = TRUE,
                    titleY = NULL, minY = NULL, maxY = NULL, logY = FALSE, 
                    titleX = NULL, minX = NULL, maxX = NULL,
                    nchart = NULL, width_div = "10%",
                    height_px = 500, width_facet_p = 10/100, width_yaxis_px = 50,
                    indic_title = TRUE,
                    indic_title_var = "indic_name",
                    plot_type = "",
                    subgroup_count = 1) {
  #- dim01 dim02 ... space
  #- cht11 cht12 ... ind01
  #- cht21 cht22 ... ind02
  #- ..... ..... ... .....
  #- chtn1 chtn2 ... ind0n
  
  # This is hard-coded for the moment
  width_yaxis_p <- 6/100
  dims <- unique(plotDataChart$dimension)
  inds <- unique(plotDataChart$indic_name)
  height_px <- height_px + (100*(length(inds)-1))
  #plotDataChart$indic_title <- plotDataChart[[indic_title_var]]

  # Here we determine what the indicator title for
  # the end of the row should be (long or not long name)
  
  
  # ******** Width-related *********
  
  # yaxis_label(6%) plot() margin plot() gap indicators
  
  # for the margin on right of plots.
  width_margin <- 0.03
  extra_width <- width_margin * length(dims)
  
  
  # if there is a real title for the y-axis then add space on y-side
  width_yaxis_p <- ifelse(!is.null(titleY) && titleY != "", width_yaxis_p, 0)
  
  # This is the width of each facet that is converted to a
  # percent later. It is 100% - 10% (for the indicator title) - 
  # the yaxis title space (if used.)
  width_dim <- (1 - width_yaxis_p - extra_width - width_facet_p)/ length(dims)
  
  
  # This is the width of the plots plus the margin between them 
  # if more than one
  width_plots <- (width_dim * length(dims)) + width_margin * (length(dims)-1)
  
  # This is the width of the top and bottom titles which
  # is the total plot width plus the yaxis gap (if exists)
  width_titles <- width_plots + width_yaxis_p
  
  
  height_margin <- 0.03
  extra_height <- height_margin * (length(inds)-1)
  
  # This is the margin at top of the indicator titles
  ind_title_marg <- 0.20/length(inds) - extra_height
  if(ind_title_marg<0.01) ind_title_marg <- 0.03
  
  
  # This the background color for indicator title
  #bgindicCol <- ifelse(indic_title, "#e2dddd", "white")
  bgindicCol <- "white"
  
  
  
  # The height of the charts is 500px divided by number of
  # indicators
  height_px_per_chart <- (height_px - extra_height)/length(inds)
  
  
  # validate there are a squere number of charts
  # this is horrendous coding and can be cleaned with more time
  # but the indic_title piece is needed to allow long/non-long
  # indicators
  
  if("indic_title"%in%names(plotDataChart)){
    indic_titles <- distinct(plotDataChart, indic_name, indic_title)
    plotDataChart <- completeCombinations(plotDataChart)
    plotDataChart$indic_title <- NULL
    plotDataChart <- left_join(plotDataChart, indic_titles, by = "indic_name")
  }
  
  
  
  # Change the size, axes etc of each chart
  plotDataChart$chart <- map(plotDataChart$chart, hc_size, height = height_px_per_chart)
  
  
  if(plot_type == "plotDetailBar_explore_hc" && subgroup_count > 20) {
    
    plotDataChart$chart <- map(plotDataChart$chart, hc_size, height = height_px + subgroup_count * 10)
  }
  
  if(logY) {
    
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, type = "logarithmic")
    #plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, min = 0.01)
    # plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, 
    #                            max = log(plotDataChart$chart[[1]]$x$hc_opts$yAxis$max))
  }
  
  if(!is.null(minY) && minY != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, min = minY)
  }
  
  if(!is.null(maxY) && maxY != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, max = maxY)
  }
  
  
  if(!is.null(minX) && minX != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, min = minX)
  }
  
  if(!is.null(maxX) && maxX != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, max = maxX)
  }
  
  # Here we send the modal
  if(!is.null(nchart)) {
    
    tmpTitleX <- titleX
    tmpTitleY <- titleY
    
    # Not sure why there is an issue here?
    if(plot_type%in%c("plotDisagLine_explore_hc", 
                      "plotDisagLine_compare_hc")){
      tmpTitleX <- titleY
      tmpTitleY <- titleX
    }
    
    # Create the various titles
    
    modal_chart <- plotDataChart[nchart,]
    if(plot_type%in%c("plotDisagLine_explore_hc", 
                      "plotDisagBar_explore_hc")){
      mod_title <- paste0(modal_chart$indic_name, " by ", tolower(modal_chart$dimension))
      mod_subtitle <- title
    }
    
    
    # for detailed bar it's turned on its side so indicator is labeled
    # dimension
    if(plot_type%in%c("plotDetailBar_explore_hc")){
      mod_title <- paste0(modal_chart$dimension, " by ", tolower(modal_chart$indic_title))
      mod_subtitle <- title
    }
    
    
    
    # Here We change the titles for the modal
    if(plot_type%in%c("plotSummaryBar_explore_hc", 
                      "plotSummaryLine_explore_hc")){
      
      modifier <- ifelse(.rdata[['HEATversion']] == "whodata", "Within-country", "Within-setting")
      mod_title <- paste0(modifier, " inequality (according to ", tolower(modal_chart$dimension2), ")")
      mod_subtitle <- title
    }
    
    if(plot_type%in%c("plotDisagLine_compare_hc")){
      
      set <- ifelse(modal_chart$ncountry == 1, " setting", " settings")
      coun <- ifelse(modal_chart$ncountry == 1, " country", " countries")
      modifier <- ifelse(.rdata[['HEATversion']] == "whodata", coun, set)
      
      mod_title <- paste0(modal_chart$indic_name, " by ", tolower(modal_chart$dimension2), " in ", 
                          modal_chart$ncountry, modifier)
      mod_subtitle <- ""
    }
    
    
    if(plot_type%in%c("plotSummaryScatter_compare_hc")){
      
      val <- ifelse(.rdata[['HEATversion']] == "whodata", "country", "setting")
      set <- ifelse(modal_chart$ncountry == 1, " setting", " settings")
      coun <- ifelse(modal_chart$ncountry == 1, " country", " countries")
      modifier <- ifelse(.rdata[['HEATversion']] == "whodata", coun, set)
      
      mod_title <- paste0(modal_chart$indic_name2, ": national average and within-", val,
                          " inequality (according to ", tolower(modal_chart$dimension2), ")", " in ", 
                          modal_chart$ncountry, modifier)
      mod_subtitle <- ""
    }
    
    
    
    modal_chart <- plotDataChart[nchart,]
    
    hc <- modal_chart$chart[[1]] %>% 
      hc_xAxis(title = list(text = tmpTitleX)) %>% 
      hc_yAxis(title = list(text = tmpTitleY)) %>% 
      hc_title(text = mod_title) %>% 
      hc_subtitle(text = mod_subtitle)
    
    #hc <- addJustify(hc)
    
    return(hc)
    
  }
  
  # if you have more than 2 dimesions then rotate
  rotation <- ifelse(length(dims)>2 , -45, 0)
  rotation <- ifelse(plot_type == "plotDetailBar_explore_hc", 0, rotation)
  plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, labels = list(rotation = rotation))
  # fix for bar (flip) charts
  plotDataChart$chart <- map_if(plotDataChart$chart,
                                function(hc){ !is.null(hc$x$hc_opts$chart$type) && hc$x$hc_opts$chart$type == "bar"  && plot_type != "plotDetailBar_explore_hc"},
                                hc_xAxis, labels = list(rotation = 0))
  
  
  # Using map2 allows you apply a function at the list-element level
  # so chart 1 and n as 1, chart 2, 2 etc. In this case
  # we're adding the button and the click
  
  plotDataChart$chart <- map2(plotDataChart$chart, seq_along(plotDataChart$chart), function(hc, n){
    
    jsfun <- "function() {
    $('#zoomhc').find('div').empty()
    console.log('click', %s); 
    /* the random hack :) always send and different input so force the renderHighcarts trigger */
    Shiny.onInputChange('nchart', (%s + Math.random()));
  }"
    
    
    jsfun <- sprintf(jsfun, n, n)
    
    # https://jsfiddle.net/jbkunst/m8szsfsm/
    # http://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/navigation/buttonoptions-theme/
    hc <- hc %>% 
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            theme = list(
              "fill-opacity" = 0,
              states = list(
                hover = list(
                  fill = "rgba(192,192,192,0.3)"
                )
              )
            ),
            symbolX = 12.5 + 7,
            symbolY = 10.5 + 7,
            menuItems = NULL,
            symbol = 'url(expandicon.ico)',
            useHTML = TRUE,
            onclick = JS(jsfun)
          )  
        )
      )
    
    "%"
    
    jsfun2 <- JS("function(){
                 console.log(this);
                 this.setSize('100%');
                 this.reflow();
    }")
    
    hc <- hc %>% 
      hc_chart(
        events = list(
          beforePrint = jsfun2
        )
      )
    
    hc
    
  })  
  
  
  grid <- plotDataChart %>% 
    group_by(indic_name) %>%
    # within each indicator name we take all the charts (for example, there
    # might be two charts for indicator ABC because the user selected two
    # dimensions). Each chart is wrapped in a div with map and becomes a list
    # item. Once done, a new div with the title is added as the final list item
    do(rowcharts = {
      d <- .
      # here we put a div around each chart
      map(d$chart, div, class = "box chartbox", style = sprintf("width:%s", percent(width_dim))) %>%
        append(
          list(
            
            div(unique(d$indic_title), class = "facet facetind box titlebox", style = sprintf("width:%s;margin-top:%s;background-color:%s", percent(width_facet_p), percent(ind_title_marg),  bgindicCol))
          )
        )
      
    })  
  
  
  # each grid$rowchart is a list with the charts for one indicator
  # and the last list item is the title. This piece will pre-pend
  # div(s) for the dimension names/titles and then a blank box div
  # so here is a set of divs for a 2 x 2
  # <div class="facet facetdim box" style="width:45%">Economic status</div> 
  # <div class="facet facetdim box" style="width:45%">Education</div>
  # <div class="box" style="width:10%"></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-88888"></div></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-55555"></div></div>
  # <div class="facet facetind box" style="width:10%">title</div>
  # <div class="box" style="width:45%"><div id="htmlwidget-88888"></div></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-55555"></div></div>
  # <div class="facet facetind box" style="width:10%">title</div>

  # graph title head
  title_ht <- ifelse(plot_type == "plotDetailBar_explore_hc" & length(dims) >1, "60px", "30px")
  
  grid <- grid$rowcharts %>%
    append(
      append(
        list(
          map(dims, div,
              class = "facet facetdim box",
              style = sprintf("height:%s;width:%s", title_ht,percent(width_dim)))
        ),
        list(
          div("", class = "box", style = sprintf("width:%s", percent(width_facet_p)))
        )
      ),
      . # note here is the previous grid
    )
  
  
  
  
  if(!is.null(titleY) && titleY != "") {
    grid <- addTitleY(grid, titleY, width_yaxis_px, height_px)
  }
  
  if(!is.null(title) && title != "") {
    grid <- addTitle(grid, title, width_titles)
  }
  
  if(!is.null(titleX) && titleX != "") {
    grid <- addTitleX(grid, titleX, width_titles)
  }
  
  if(legend){
    grid <- addLegend(grid, plotDataChart, width_titles)
  }
  
  
  #browsable(div(style=sprintf("width:%spx", width_div), grid))
  
  browsable(grid)
}

completeCombinations <- function(plotDataChart) {
  
  dims <- unique(plotDataChart$dimension)
  inds <- unique(plotDataChart$indic_name)
  
  
  
  plotDataChart2 <- expand.grid(indic_name = inds, 
                                dimension = dims, stringsAsFactors = FALSE) %>% 
    tbl_df() %>% 
    mutate(indic_name = factor(indic_name, levels = inds),
           dimension = factor(dimension, levels = dims)) %>% 
    arrange(indic_name, dimension) %>% 
    dmap_if(is.factor, as.character) %>% 
    left_join(plotDataChart, by = c("indic_name", "dimension"))
  
  plotDataChart2$chart <- map(plotDataChart2$chart, function(x){
    if(is.null(x)){
      res <- highchart()
    } else {
      res <- x
    }
    res
  })
  
  plotDataChart <- plotDataChart2
  
  plotDataChart
}

addTitle <- function(grid, title, width){
  
  grid <- tagList(
    
    div(class = "box titlehc", style = sprintf("width:%s", percent(width)), title),
    grid
  )
  
  grid
  
}


addTitleY <- function(grid, titleY, width_yaxis_px, height_px){
  #Ebrowser()
  height_px <- height_px + ((height_px-500)/100)*25
  styles1 <- sprintf("width:%spx", width_yaxis_px)
  styles2 <- sprintf("width: calc(100%% - %spx);",100, width_yaxis_px)
  
  hc <- highchart(height=height_px) %>% 
    hc_xAxis(visible = FALSE) %>% 
    hc_yAxis(
      title = list(text = titleY, style = list(fontSize = "1.25em")),
      labels = list(enabled = FALSE),
      startOnTick = FALSE,
      endOnTick = FALSE,
      tickPositions = list()
    ) %>% 
    hc_tooltip(enabled = FALSE) %>% 
    hc_add_series(data = c(0, 0), color = "transparent", showInLegend = FALSE) 
  
  
  grid <- div(class = "grid box",
              div(class = "grid box", style = styles1, hc),
              div(class = "grid box", style = styles2, grid)
              
  )
  
  grid
  
}

addTitleX <- function(grid, titleX, width ){
  
  grid <- tagList(
    grid,
    div(class = "box facet facetdim titlex", style = sprintf("width:%s", percent(width)), titleX)
  )
  
  grid
  
}

addLegend <- function(grid, plotDataChart, width){
  
  grid <- tagList(
    grid,
    div(class = "box legendbox", style = sprintf("width:%s", percent(width)), getLegend(plotDataChart))
  )
  
  grid
}

getLegend <- function(plotDataChart) {
  
  dfseries <- plotDataChart$chart %>% 
    map(function(x){
      x$x$hc_opts$series
    }) %>% 
    unlist(recursive = FALSE) %>% 
    map_df(function(x){
      data_frame(
        dimension = ifelse(is.null(x$data[[1]]$dimension), NA, x$data[[1]]$dimension),
        name = ifelse(is.null(x$name), NA, x$name),
        type = x$type,
        color = x$color)
    }) %>% 
    filter(!is.na(name)) %>% 
    filter(name != "range") 
  
  #browser()
  #tmp <- distinct(dfseries, deimsniosn)
  
  # dfseries <- dfseries %>% group_by(dimension, name) %>% 
  # mutate(count = n()) %>% ungroup 
  
  # dfseries$name[dfseries$count>7] <- 
  # dfseries$dimension[dfseries$count>7]
  
  dfseries <- select(dfseries, -dimension) %>% distinct
  
  highchart() %>% 
    hc_add_series(data = c(0, 0), color = "transparent", showInLegend = FALSE) %>% 
    hc_add_series_list(list_parse(dfseries)) %>% 
    hc_size(height = 100) %>% 
    hc_xAxis(visible = FALSE) %>% 
    hc_yAxis(visible = FALSE) %>% 
    hc_chart(spacing = rep(10, 4)) %>% 
    hc_tooltip(enabled = FALSE) %>% 
    hc_plotOptions(
      series = list(
        events = list(legendItemClick = JS("function(){ return false;}"))
      )
    ) %>% 
    hc_legend(enabled = TRUE, padding = 0, margin = 0, verticalAlign = "middle",
              # 36
              align = "center")
  
}
