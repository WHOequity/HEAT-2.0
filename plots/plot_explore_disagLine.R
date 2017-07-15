

#######PLOT 2
plotDisagLine_explore_ggplot <- function(plotData){
  #  Plot 2: Horizontal line chart for a single country (Disaggregation of data)

  # this is here to trigger a change if the title changes

  

  # git681, flash
  isolate({
    xtitle_val <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
    ytitle_val <- ifelse(!is.null(input$yaxis_title1) && input$yaxis_title1 !="", input$yaxis_title1, "")
    
  })

  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismax <- isolate(input$axis_limitsmax1)
  
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  

  
  

  dat1 <- filter(plotData, !dimension%in%.rdata[['geo_dimension']]) %>% ungroup
  dat1$subgroup <- as.character(dat1$subgroup)
  dat2 <- filter(plotData, dimension%in%.rdata[['geo_dimension']]) %>% ungroup
  dat2$subgroup <- as.character(dat2$subgroup)

  # see git846, change names to numbers because names get repeated
  if(nrow(dat2)>0){
  dat2 <- arrange(dat2, dimension, subgroup)
  dat2$subgroup <- 1:nrow(dat2)
  }
  
  mydat<-rbind(dat1, dat2)
  mydat$subgroup<-factor(mydat$subgroup, levels=unique(mydat$subgroup))
  
  
  dat1info<-select(dat1, subgroup, colors, shapes) %>% distinct

  
  cols<-NULL
  shapes<-NULL
  breaks<-NULL
  labels<-NULL
  alpha <- NULL
  if(nrow(dat1)>0){
    cols <- c(cols, dat1info$colors)
    shapes <- c(shapes, dat1info$shapes)
    breaks <- c(breaks, unique(as.character(dat1$subgroup)))
    labels <- c(labels, unique(as.character(dat1$subgroup)))
    alpha <- c(alpha, rep(0.75, nrow(dat1info)))
  } 
  
  if(nrow(dat2)>0){
    cols <- c(cols, dat2$colors)
    shapes <- c(shapes, dat2$shapes)
    breaks <- c(breaks, as.character(dat2 %>% group_by(dimension) %>% slice(1) %>% .$subgroup))
    #breaks <- c(breaks, as.character(dat2$subgroup[1:length(unique(dat2$dimension))]))
    labels <- c(labels, as.character(unique(dat2$dimension)))
    alpha <- c(alpha, rep(0.4, nrow(dat2)))
    
  } 
  
  
  

  
  p<-ggplot(data = mydat, aes(x=estimate, y=as.factor(year)))+
    geom_line() +
    geom_point(aes(fill=subgroup, shape=subgroup, alpha=subgroup), color="grey", size=4) +
    scale_shape_manual(values= shapes, 
                       breaks=breaks,  
                       name="", 
                       labels=labels)+
    scale_fill_manual(breaks=breaks, 
                      values = cols,
                      name="",
                      labels=labels) +
    scale_alpha_manual(values=alpha, guide = 'none')
  
  
  form <- "indic_name ~ dimension"
  if(!is.null(input$long_names1) && input$long_names1 == FALSE) form <- "indic ~ dimension"
  
  
  p <- p+
    facet_grid(form, scale="free_y", space = "free_y", labeller = splitLabels)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    heat_theme()+
    guides(shape = guide_legend(nrow=5,byrow=FALSE))
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$layout$panel_ranges[[1]]$x.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(xlim=c(axismin, axismax))
    
  }
  
  
  
  return(p)
}

################################




