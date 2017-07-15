output$ui_collapse_compare_sum_plot <- renderUI({
  
  tags$div(class="panel-group",
           tags$div(class="panel panel-default",
                    tags$div(class="panel-heading",
                             
                             tags$a(`data-toggle`="collapse", `data-target`="#collapse_compare_sum_plot", 
                                    class="collapsesectionhead collapsed cursor-pointer", "Graph options")
                    ),
                    tags$div( id="collapse_compare_sum_plot", class="panel-collapse collapse",
                              tags$div(class="panel-body",
                                       
                                       
                                             
                                       
                                       
                              #conditionalPanel(condition = "input.summary_plot_mode_compare == 'ggplot'",
                                         uiOutput("summary_plot_dimensions_compare"),#),
                              tags$span(class="control-label axis-range", "Select axis range"),
                                             
                                             tags$div(class="axis-minmax-dtl",
                                                      textInputRow(inputId="xaxis_limitsmin4", label = "Horizontal axis minimum", value = NULL),
                                                      textInputRow(inputId="xaxis_limitsmax4", label = "Horizontal axis maximum", value = NULL),
                                                      textInputRow(inputId="yaxis_limitsmin4", label = "Vertical axis minimum", value = NULL),
                                                      textInputRow(inputId="yaxis_limitsmax4", label = "Vertical axis maximum", value = NULL)
                                             ),
                                             tags$span(class="control-label graph-names", "Select graph names"),
                                             
                                             
                                             tags$div(class="axis-title-label",
                                                      textInput(inputId = 'main_title4', label = 'Main title', value = ""),
                                                      textInput(inputId = 'xaxis_title4', label = 'X-axis label', value = ""),
                                                      textInput(inputId = 'yaxis_title4', label = 'Y-axis label', value = "")
                                             )
                              )
                    )
           )
  )
  
})

