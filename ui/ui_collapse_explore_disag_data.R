output$ui_collapse_explore_disag_data <- renderUI({tags$div(class="panel-group",
         tags$div(class="panel panel-default",
                  tags$div(class="panel-heading",
                           
                           tags$a(`data-toggle`="collapse", `data-target`="#collapse_explore_disag_data", 
                                  class="collapsesectionhead collapsed cursor-pointer", "Table options")
                  ),
                  tags$div( id="collapse_explore_disag_data", class="panel-collapse collapse",
                            tags$div(class="panel-body",
                                     uiOutput('dataTableItems_explore'),
                                     sliderInput('sumsigfig', 'Select number of decimals', min=0, max=5, value=1, round=T, width='100%')
                            )
                  )
         )
)
})