output$ui_collapse_explore_sum_data <- renderUI({
  list(
    # commented out per git 678
    # tags$div(class="panel-group",
    #          tags$div(class="panel panel-default",
    #                   tags$div(class="panel-heading",
    #                            
    #                            tags$a(`data-toggle`="collapse", href="#collapse_explore_sum_data1", class="collapsesectionhead", "Summary measure options")
    #                   ),
    #                   tags$div( id="collapse_explore_sum_data1", class="panel-collapse collapse",
    #                             tags$div(class="panel-body",
    #                                      uiOutput("summary_measures")
    #                             )
    #                   )
    #          )
    # ),
    
    tags$div(class="panel-group",
             tags$div(class="panel panel-default",
                      tags$div(class="panel-heading",
                               
                               tags$a(`data-toggle`="collapse", `data-target`="#collapse_explore_sum_data2", 
                                      class="collapsesectionhead collapsed cursor-pointer", "Table options")
                      ),
                      tags$div( id="collapse_explore_sum_data2", class="panel-collapse collapse",
                                tags$div(class="panel-body",
                                         uiOutput("dataTableItemsSummary_explore"),
                                         sliderInput('sumsigfig2', 'Select estimate precision', min=0, max=5, value=1, round=T, width='100%')
                                )
                      )
             )
    )
  )
})