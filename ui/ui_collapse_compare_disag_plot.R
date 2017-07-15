output$ui_collapse_compare_disag_plot <- renderUI({
  
  tags$div(class="panel-group",
           tags$div(class="panel panel-default",
                    tags$div(class="panel-heading",
                             
                             tags$a(`data-toggle`="collapse", `data-target`="#collapse_compare_disag_plot", class="collapsesectionhead collapsed cursor-pointer", "Graph options")
                    ),
                    tags$div( id="collapse_compare_disag_plot", class="panel-collapse collapse",
                              tags$div(class="panel-body",
                                       
                                       
                                       # These sliders were originally in server_logic but were not getting
                                       # loaded if the compare tab was clicked first. Seems that the split second
                                       # that the condition was not TRUE caused the sliders not to show up
                          
                                       
                                       
                                      #conditionalPanel(condition = "input.disag_plot_mode_compare == 'ggplot'",
                                                                     sliderInput('plot_height3', 'Select graph height', 
                                                                                 min=200, max=1500, value=650, step = 100,
                                                   round = T,
                                                   ticks = TRUE, animate = FALSE),
                                       
                                       sliderInput('plot_width3', 'Select graph width', min=200, max=1500, value=650, step = 100,
                                                   round = T,
                                                   ticks = TRUE, animate = FALSE),#),
                                       
                                       tags$span(class="control-label axis-range", "Select axis range"),
                                       tags$div(class="axis-minmax",
                                                textInputRow(inputId="axis_limitsmin3", label="Axis minimum", value = NULL),
                                                textInputRow(inputId="axis_limitsmax3", label="Axis maximum", value = NULL)
                                       ),
                                       tags$span(class="control-label graph-names", "Select graph names"),
                                       
                                       
                                       #checkboxInput(inputId='long_names3', label='Use long health indicator names', value = TRUE),
                                       tags$div(class="axis-title-label",
                                                textInput(inputId = 'main_title3', label = 'Main title', value = "Health Equity Summary"),
                                                textInput(inputId = 'xaxis_title3', label = 'Horizontal axis title', value = ""),
                                                textInput(inputId = 'yaxis_title3', label = 'Vertical axis title', value = "")
                                       )
                              )
                    )
           )
  )
  
})

