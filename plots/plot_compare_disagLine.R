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

plotDisagLine_compare_ggplot <- function(plotData, session){
  #  Plot 5: Horizontal line chart for benchmark countries (Disaggregation of data)
  
  #input$main_title3

  isolate({
    xtitle_val <- ifelse(!is.null(isolate(input$xaxis_title3)) && 
                           isolate(input$xaxis_title3) !="", isolate(input$xaxis_title3), "")
    ytitle_val <- ifelse(!is.null(isolate(input$yaxis_title3)) && 
                           isolate(input$yaxis_title3) !="", isolate(input$yaxis_title3), "")
  })

  
  
  
  plotData$country <- paste0(plotData$country, " (", plotData$source, " ", plotData$year, ")")
  plotData <- dplyr::arrange(plotData, anchor, desc(country))%>% ungroup
  plotData <- mutate(plotData,
                     country = factor(country, levels=unique(country)))
  
  
  maintitle_val <- .rdata[['plotDisag_compare_title']]
  
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  axismin <- isolate(input$axis_limitsmin3)
  axismax <- isolate(input$axis_limitsmax3)
  
  #p <- ggplot(plotData, aes(estimate, country)) + geom_line()
  
  datainfo<-select(plotData, subgroup, colors, shapes) %>% distinct
  
  indictype <- "indic_name"
  p<-ggplot(plotData, aes(estimate, country)) + geom_line()
  
  #browser()
  
  if(!plotData$dimension[1] %in% .rdata[['geo_dimension']]){
    p <- p + geom_point(aes(fill=subgroup, shape=subgroup, alpha = subgroup), size=4, color="grey")
    p<- p+ scale_shape_manual(values= datainfo$shapes, 
                              breaks=datainfo$subgroup,  
                              name="", 
                              labels=datainfo$subgroup)+
      scale_fill_manual(breaks=datainfo$subgroup, 
                        values = datainfo$colors,
                        name="",
                        labels=datainfo$subgroup)+
      guides(shape = guide_legend(nrow=5,byrow=FALSE))+
      scale_alpha_manual(values=rep(0.75, nrow(plotData)), guide = 'none')
    
  }
  

  if(plotData$dimension[1] %in% .rdata[['geo_dimension']]){
    
    p <- p + geom_point(aes(fill=subgroup), size=4, alpha=0.5, color=datainfo$colors[1])+
      scale_fill_manual(breaks=as.character(datainfo$subgroup[1]), 
                        values = datainfo$colors,
                        name="",
                        labels=plotData$dimension[1])
    
  }
  
  
  
  p<-p+
    labs(x=xtitle_val, y=ytitle_val, title=maintitle_val)+
    facet_grid(paste0(".~", indictype), labeller = splitLabelsWide)+
    heat_theme()+
    theme(legend.justification = 'center',
                       strip.text = element_blank(),
                      strip.text.x = element_blank(),
          strip.text.y = element_blank(),
                       strip.background = element_rect(fill="white"))
  

  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$x.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(xlim=c(axismin, axismax))
    
  }
  
  
  
  
  
  
  return(p)
  
}


