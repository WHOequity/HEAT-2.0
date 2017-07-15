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
plotDisagBar_explore_ggplot <- function(plotData, session){
  # Plot 1: Barchart for a single country (Disaggregation of data)
  # this is here to trigger a change if the title changes
  if(is.null(input$disag_error_bars)) return()
  
  isolate({
    #input$main_title1
    xtitle_val <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
    ytitle_val <- ifelse(!is.null(input$yaxis_title1) && input$xaxis_title1 !="", input$yaxis_title1, "")
    
  })

  
  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismax <- isolate(input$axis_limitsmax1)
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]], fixed=FALSE)
  
  
  
  numyears<-length(unique(plotData$year))
  binwidth <- ifelse(numyears>2, 0.75, 0.35)
  textwidth <- ifelse(numyears>2, 0.9, 0.35)
  
  
  
 # form <- ifelse(input$long_names1, "indic_name ~ dimension", "indic ~ dimension")
  
  dat1 <- filter(plotData, !dimension%in%.rdata[['geo_dimension']]) %>% ungroup
  dat2 <- filter(plotData, dimension%in%.rdata[['geo_dimension']]) %>% ungroup
  
  dat1info<-select(dat1, subgroup, colors, shapes) %>% distinct
  
  cols<-NULL
  shapes<-NULL
  breaks<-NULL
  labels<-NULL
  if(nrow(dat1)>0){
    cols <- c(cols, dat1info$colors)
    shapes <- c(shapes, dat1info$shapes)
    breaks <- c(breaks, unique(as.character(dat1$subgroup)))
    labels <- c(labels, unique(as.character(dat1$subgroup)))
  } 
  
  if(nrow(dat2)>0){
    cols <- c(cols, dat2$colors)
    shapes <- c(shapes, dat2$shapes)
    breaks <- c(breaks, as.character(dat2 %>% group_by(dimension) %>% slice(1) %>% .$subgroup))
    labels <- c(labels, unique(dat2$dimension))
    
  } 

  #mydat<-rbind(dat1, dat2)
  
  
  #mydat <- mydat %>% arrange(dimension, order)
  #mydat$subgroup<-factor(mydat$subgroup, levels=unique(mydat$subgroup))
  
  

  
  p<-ggplot() +
    geom_bar(data=plotData, aes(x = as.factor(year), weight = estimate, fill=subgroup, y=estimate), 
             position=position_dodge(), stat="identity", color='white', width=binwidth)+
    geom_text(data=plotData, aes(x = as.factor(year), y=estimate,fill=subgroup, 
                                 label=round(estimate),ymin=lower_95ci, ymax=upper_95ci), 
              position=position_dodge(width=binwidth), vjust=-0.75,
              size=3, color="grey50", alpha=!input$disag_error_bars)+
    geom_errorbar(data=plotData, aes(x = as.factor(year), y=estimate,fill=subgroup, 
                                     label=round(estimate),ymin=lower_95ci, ymax=upper_95ci), color="grey50",  
                  position=position_dodge(width=binwidth), width=0.25, alpha=input$disag_error_bars) + 
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    #ylim(as.integer(input$axis_limitsmin1),  as.integer(input$axis_limitsmax1))+
    heat_theme()+
    scale_fill_manual(breaks=breaks, 
                      values=cols, name="",
                      labels=labels)
  # guides(fill = guide_legend(nrow=2,byrow=TRUE, override.aes = list(linetype=0)))
  
    form <- "indic_name ~ dimension"
  if(!is.null(input$long_names1) && input$long_names1 == FALSE) form <- "indic ~ dimension"
  
  
  p <- p+
    facet_grid(form, scale="free_y",  labeller = splitLabels)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    heat_theme()+
    guides(fill = guide_legend(nrow=5,byrow=FALSE, override.aes = list(linetype=0)))
  
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }
  
  
  
  return(p)
}

