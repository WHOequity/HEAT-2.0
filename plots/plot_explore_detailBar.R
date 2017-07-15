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


#######PLOT 1
plotDetailBar_explore_ggplot <- function(plotData, sortBy = NULL, regs = NULL, 
                                         showAVG = TRUE, showMedian = TRUE, 
                                         addGroupNames = TRUE, indicSort = NULL, ...) {
  # Plot 1: Barchart for a single country (Disaggregation of data)
  # this is here to trigger a change if the title changes
  
  #xtitle_val <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
  #ytitle_val <- ifelse(!is.null(input$yaxis_title1) && input$xaxis_title1 !="", input$yaxis_title1, "")
  
  isolate({
    #input$main_title_dtl
    ytitle_val <- ifelse(!is.null(input$xaxis_title_dtl) && input$xaxis_title_dtl !="", input$xaxis_title_dtl, "")
    xtitle_val <- ifelse(!is.null(input$yaxis_title_dtl) && input$yaxis_title_dtl !="", input$yaxis_title_dtl, "")
    longnames <- input$long_names_dtl
  })

  

  maintitle_val <- .rdata[["plotDisag_explore_title_dtl"]]
  
  axismin <- isolate(input$axis_limitsmin_dtl)
  axismax <- isolate(input$axis_limitsmax_dtl)
  
  
  #axismin <- isolate(input$axis_limitsmin1)
  #axismax <- isolate(input$axis_limitsmax1)
  
  isolate({
    disag_plot_explore_dtl_sort      <- input$disag_plot_explore_dtl_sort
    disag_plot_explore_dtl_showAVG   <- input$disag_plot_explore_dtl_showAVG
    disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
    descend <- input$sortOrder_ind_dim == "Descending"
    disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
    disag_plot_explore_dtl_showNames <- input$disag_plot_explore_dtl_showNames
    disag_indicSort <- input$focus_indicator_explore_plotdtl[input$focus_indicator_explore_plotdtl%in%unique(plotData$indic)]
    indics <- input$focus_indicator_explore
  })
  
  if(.rdata[['is_modal']]){
    sortBy  <- disag_plot_explore_dtl_sort
    regs    <- disag_plot_explore_dtl_subgroups
    showAvg <- disag_plot_explore_dtl_showAVG
    showMedian <- disag_plot_explore_dtl_showMedian
    addGroupNames <- disag_plot_explore_dtl_showNames
    indicSort <- disag_indicSort
  }
  
  
  plotData <- plotData %>% group_by(indic_name) %>% 
    mutate(median_estimate = round(median(estimate, na.rm=T), 2),
           range_estimate = abs(diff(range(estimate)))) %>% 
    ungroup
  

  levels1 <- levels(plotData$subgroup)
  allsubs <- filter(.rdata$maindata, country == plotData$country[1], year == plotData$year[1],
                    dimension == plotData$dimension[1], source %in% unique(plotData$source)) %>% .$subgroup %>% unique
  missing_subs <- as.character(allsubs[!allsubs%in%levels1])

  line_dat <- plotData %>%  count(indic_name, median_estimate, range_estimate, national)
  
  
  
  indicOrder <- plotData %>% select(indic, indic_name) %>% distinct %>% 
    mutate(indic = factor(indic, levels = indics)) %>% 
    arrange(indic)
  

  # The descend vs non descend are backward here because ggplot2
  # orders from bottom to top
  if(!is.null(sortBy) && sortBy != "" && sortBy == "subgroup") {
    
    if(descend){
      plotData$subgroup <- factor(plotData$subgroup, 
                                  levels = sort(c(as.character(unique(plotData$subgroup)), missing_subs)))
      #plotData <- arrange(plotData, indic_name, subgroup)
    }else{
      plotData$subgroup <- factor(plotData$subgroup, 
                                  levels = rev(sort(c(as.character(unique(plotData$subgroup)), missing_subs))))
    }
    
  }
  

  
  if(!is.null(sortBy) && sortBy != "" && sortBy != "subgroup") {
    
    if(descend){
      levels1 <- filter(plotData, indic == sortBy) %>%
        select(subgroup, estimate) %>%
        arrange_("estimate") %>%
        .$subgroup %>% as.character
      missing_subs <- as.character(allsubs[!allsubs%in%levels1])
    }else{
      levels1 <- filter(plotData, indic == sortBy) %>%
        select(subgroup, estimate) %>%
        arrange_("desc(estimate)") %>%
        .$subgroup %>% as.character
      missing_subs <- as.character(allsubs[!allsubs%in%levels1])
    }
    

    plotData$subgroup <- factor(plotData$subgroup, levels = c(levels1, missing_subs))
  }


 
  # make sure factor levels are set up properly
  plotData <- mutate(plotData,
                     indic = factor(indic, levels = indicOrder$indic),
                     indic_name = factor(indic_name, levels = indicOrder$indic_name))
  
  plotData <- arrange(plotData, indic_name, subgroup)
  
  form <- "dimension~indic_name"
  if(!is.null(longnames) && longnames == FALSE) form <- "dimension~indic"
  
  
  
  plotData$colors[plotData$subgroup%in%regs] <- "red"
  
  wrap_val <- switch(nrow(indicOrder),
                     "1" = 40,
                     "2" = 25,
                     "3" = 15)

  width <- 0.5
  if(line_dat$n[1] == 1) width <- 0.25
  
  p <- ggplot(plotData, aes(subgroup, estimate)) + 
    geom_bar(stat='identity', fill = plotData$colors, width = width) + 
    coord_flip()+
    scale_x_discrete(drop = FALSE) + 
    facet_grid(form, scales = "free_x", labeller = labeller(indic_name = label_wrap_gen(wrap_val)),
               as.table = FALSE) + 
    labs(title=maintitle_val, x = xtitle_val, y = ytitle_val) +
    heat_theme() +
    theme(strip.text.y = element_text(hjust = 0.5, vjust = 0.5, angle=-90, margin = margin(10,10,10,10)))
  
  if(showMedian){
    p <- p + geom_hline(data = line_dat, aes(yintercept = median_estimate), size = 0.25, color = "#ee7600") +    
      geom_text(data = line_dat, aes(y=median_estimate, x = max(line_dat$n), 
                    label = paste("\nMedian: ", median_estimate)), 
                angle = -90,
                hjust = 0,
                color = "#ee7600", lineheight = 0.85) 
  }
  
  if(showAVG){

    p <- p + 
      geom_hline(data = line_dat, aes(yintercept = national), size = 0.25) +    
      
      geom_text(data = line_dat, aes(y = national, x = max(n),
                    label = paste("\nSetting average: ", national)), angle = -90, hjust = 0,
                color = "black", lineheight = 0.85)
  }
  
  

  if(axismin!="" | axismax != "") {

    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$x.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_flip(ylim=c(axismin, axismax))

  }
  
  
  return(p)
}

