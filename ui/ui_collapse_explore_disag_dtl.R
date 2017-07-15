output$longindicname_disag_explore_dtl <- renderUI({
  
  if(.rdata[['HEATversion']] == "upload"){
    li <- NULL
  }else{
    li <- list(
      checkboxInput(inputId='long_names_dtl', label='Use long health indicator names', value = TRUE))
  }
  li
})




output$disag_plot_explore_dtl_btn <- renderUI({
  
  
  tags$div(class="panel-group",
           tags$div(class="panel panel-default",
                    tags$div(class="panel-heading dtl-panel",
                             
                             tags$a(`data-toggle`="collapse", `data-target`="#collapse_explore_dtl_plot", 
                                    class="collapsesectionhead collapsed cursor-pointer", "Graph options")
                    ),
                    tags$div( id="collapse_explore_dtl_plot", class="panel-collapse collapse",
                              tags$div(class="panel-body",
                                       
                                       
                                       
                                       fluidRow(
                                         column(width = 7, class = "dtl-radio", radioButtons("sortBy_ind_dim", label = "Sort by", 
                                                                                             choices = c("Inequality dimension" = "Dimension",
                                                                                                         "Health indicator" = "Indicator" 
                                                                                             ),
                                                                                             selected = "Dimension")
                                         ),
                                         column(width = 5,           radioButtons("sortOrder_ind_dim", label = "Sort order", 
                                                                                  choices = c("Ascending", "Descending"),
                                                                                  selected = "Ascending")
                                                
                                         )
                                         
                                       ),                                       fluidRow(
                                         column(width = 12, class="longerselect",
                                                conditionalPanel("input.sortBy_ind_dim == 'Indicator'",
                                                                 selectizeInput("disag_plot_explore_dtl_sort", "Select indicator to sort by",
                                                                                choices = getFullIndic(.rdata[['focus_indicator']]),
                                                                                selected = getFullIndic(.rdata[['focus_indicator']])))
                                         )
                                       ),
                                       fluidRow(id = "bottom-dtl-row",
                                                
                                                column(width = 10,    
                                                       
                                                       checkboxInput("disag_plot_explore_dtl_showAVG",
                                                                     label = "Show setting average", value = FALSE),
                                                       checkboxInput("disag_plot_explore_dtl_showMedian",
                                                                     label = "Show median", value = TRUE)#,
                                                       # checkboxInput("disag_plot_explore_dtl_showNames",
                                                       #               label = "Show subgroup names", value = TRUE)
                                                )
                                       ),
                                       fluidRow(
                                         column(width = 12, class="longerselect", 
                                                selectInput("disag_plot_explore_dtl_subgroups", 
                                                            "Highlight subgroup", 
                                                            choices = c("Choose one..." = "", .rdata[['focus_plotdtl_subgroups']]), 
                                                            multiple = TRUE)
                                         )),
                                       #conditionalPanel(condition = "input.disag_plot_mode_explore_dtl == 'ggplot'",
                                       uiOutput("disag_plot_dimensions_explore_dtl"),#),
                                       
                                       
                                       
                                       
                                       
                                       fluidRow(
                                         column(width = 12,
                                                tags$span(class="control-label axis-range", "Select axis range"),
                                                #                        conditionalPanel("input.ai_plot_type == 'data_line'",
                                                #                                         textInputRow(inputId="axis_limitsmin1", label="Axis-min", value = NULL)
                                                #                        ),
                                                tags$div(class="axis-minmax",
                                                         textInputRow(inputId="axis_limitsmin_dtl", label="Axis minimum ", value = NULL),
                                                         textInputRow(inputId="axis_limitsmax_dtl", label="Axis maximum", value = NULL)
                                                ),
                                                tags$span(class="control-label graph-names", "Select graph names"),
                                                uiOutput("longindicname_disag_explore_dtl"),
                                                
                                                tags$div(class="axis-title-label",
                                                         textInput(inputId = 'main_title_dtl', label = 'Main title', value = "Health Equity Disaggregated"),
                                                         textInput(inputId = 'xaxis_title_dtl', label = 'Horizontal axis title', value = ""),
                                                         textInput(inputId = 'yaxis_title_dtl', label = 'Vertical axis title', value = "")
                                                )
                                         )
                                       )
                                       
                                       
                                       
                              )
                    )
           )
  )
  
  
  
  
})