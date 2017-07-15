heat_theme <- function(){
  
  
  theme(
    
    axis.title.y = element_text(vjust=1.5),
    strip.text = element_text(colour = "black", angle = 0, size = 13),
    strip.text.x = element_text(hjust = 0.5, vjust = 0.65, margin = margin(10, 0, 10, 0)),
    strip.text.y = element_text(hjust = 0.5, vjust = 0.5, angle=0),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    #axis.line = element_blank(),
    legend.spacing = unit(0.2, "cm"),
    legend.text = element_text(size=10),
    panel.spacing = unit(0.75, "cm"),
    legend.title=element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position="bottom",
    axis.ticks = element_line(color="grey70"),
    legend.key.size = unit(0.9, "lines"), # space between legend items
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    plot.title = element_text(size=15, face="bold", hjust = 0.5,
                              margin = margin(10, 0, 10, 0))
    
  )
}
