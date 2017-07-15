output$longindicname_disag_explore <- renderUI({
  
  if(.rdata[['HEATversion']] == "upload"){
    li <- NULL
  }else{
    li <- list(
    checkboxInput(inputId='long_names1', label='Use long health indicator names', value = TRUE))
  }
  li
})



output$ui_collapse_explore_disag_plot <- renderUI({
  
  tags$div(class="panel-group",
           tags$div(class="panel panel-default",
                    tags$div(class="panel-heading",
                             
                             tags$a(`data-toggle`="collapse", `data-target`="#collapse_explore_disag_plot", 
                                    class="collapsesectionhead collapsed cursor-pointer", "Graph options")
                    ),
                    tags$div( id="collapse_explore_disag_plot", class="panel-collapse collapse",
                              tags$div(class="panel-body",
                                       
                                       conditionalPanel(condition="input.disag_plot_type_explore == 'Bar'", 
                                                        uiOutput("disag_plot_error_bars")),
                                       #conditionalPanel(condition = "input.disag_plot_mode_explore == 'ggplot'",
                                         uiOutput("disag_plot_dimensions_explore"),#),
                                       
                                       
                                       
                                       tags$span(class="control-label axis-range", "Select axis range"),
                                       #                        conditionalPanel("input.ai_plot_type == 'data_line'",
                                       #                                         textInputRow(inputId="axis_limitsmin1", label="Axis-min", value = NULL)
                                       #                        ),
                                       tags$div(class="axis-minmax",
                                                textInputRow(inputId="axis_limitsmin1", label="Axis minimum ", value = NULL),
                                                textInputRow(inputId="axis_limitsmax1", label="Axis maximum", value = NULL)
                                       ),
                                       tags$span(class="control-label graph-names", "Select graph names"),
                                       uiOutput("longindicname_disag_explore"),
                                       
                                       tags$div(class="axis-title-label",
                                                textInput(inputId = 'main_title1', label = 'Main title', value = "Health Equity Disaggregated"),
                                                textInput(inputId = 'xaxis_title1', label = 'Horizontal axis title', value = ""),
                                                textInput(inputId = 'yaxis_title1', label = 'Vertical axis title', value = "")
                                       )
                              )
                    )
           )
  )
  
})