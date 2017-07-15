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

#******************************************************************************
#******************************************************************************
# FUNCTIONS TO CREATE SELECTORS
#******************************************************************************
#******************************************************************************


focusCountry_selector <- function(id, multiple=FALSE){
 
  if(is.null(.rdata[['all_countries']])) return()
  
  countries <- .rdata[['all_countries']]
  

  
  selectInput(id, 
              "Select setting (e.g. country, province, district)", 
              choices  = countries, 
              multiple = multiple, 
              selected = .rdata[['focus_country']])
}



focusIndicator_selector <- function(id, multiple = FALSE, core=FALSE, maxitems = 5,
                                    header = "Select health indicators"){


  if(is.null(.rdata[['focus_indicator']]) || trimws(.rdata[['focus_indicator']]) == ''){
    indic <- "sba"
    choices <- "sba" #git842
  }else{
    indic <- .rdata[['focus_indicator']]
    choices <- .rdata[['full_indicators']]
  }

    selectizeInput(id,
                header,
                choices  = choices,
                selected = indic,
                multiple = TRUE,
                options = list(maxItems = maxitems))
  

  
  
  
}


focusInequalType_selector <- function(id, multiple=FALSE){
  
  
  if(!any(class(.rdata[['maindata']])%in%"data.frame")){
    choice <- NULL
    sel <- NULL
  }else{
    choice <- .rdata[['summary_measures_all']]
    sel <- .rdata[["focus_inequal_type"]]
  }
  selectInput(id, 
              "Select summary measure", 
              choices= choice, 
              selected=sel, 
              multiple=multiple)
  
  
}


focusDimension_selector <- function(id, multiple = FALSE, maxitems = 5){
  
  my_label <- ifelse(maxitems == 1, "Select inequality dimension", "Select inequality dimensions")
  focus_dimen <- .rdata[['focus_dimension']]
  if(multiple) focus_dimen <- .rdata[['focus_dimension']][1]
  
  selectizeInput(inputId = id,
                 my_label,
              choices = sort(unique(.rdata[['equity_dimensions']])),
              selected = focus_dimen,
              options = list(maxItems = maxitems))
  
  
}


#******************************************************************************
#******************************************************************************
# CREATE SELECTORS, SLIDERS AND BUTTONS ---- 
#******************************************************************************
#******************************************************************************


# ----- Country selector -----------------------------------------------

output$focus_country_explore <- renderUI({
  
  focusCountry_selector("focus_country_explore")
  
})

output$focus_country_compare <- renderUI({
  
  focusCountry_selector("focus_country_compare")
  
})

# ----- Year and source selector -----------------------------------------------



output$focus_source_year_explore <- renderUI({
  
  list(
    
    selectInput("focus_data_source_explore", "Select data sources",
                 choices = .rdata[['data_sources']], #c("All", "DHS", "MICS"),
                 selected=.rdata[['focus_data_source']],
                multiple = TRUE),
    tags$label(class="control-label", "Select years"),
    checkboxInput('mostrecent_explore', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_explore",  
                      
                      selectInput(inputId="focus_year_explore", 
                                  label=NA, 
                                  choices=.rdata[['all_years']], 
                                  multiple=T, 
                                  selected=.rdata[['focus_year']])
    )
  )
})



output$focus_source_year_explore_map <- renderUI({
  
  list(
    #conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
    selectInput("focus_data_source_explore_map", "Select data sources",
                choices = .rdata[['data_sources']], #c("All", "DHS", "MICS"),
                selected=.rdata[['focus_data_source']],
                multiple = TRUE),
    
    tags$label(class="control-label", "Select year"),
    checkboxInput('mostrecent_explore_map', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_explore_map",  
                      
                      selectInput(inputId="focus_year_explore_map", 
                                  label=NA, 
                                  choices=.rdata[['all_years']], 
                                  multiple=FALSE, 
                                  selected=.rdata[['focus_year']][1])
    )
  )
})






# ----- Indicator selector -----------------------------------------------


output$focus_indicator_explore <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore", multiple=TRUE, core=FALSE)
  
  
})

output$focus_indicator_explore_map <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore_map", multiple=TRUE,
                          core=FALSE, maxitems = 3)
  
  
})

output$focus_indicator_explore_plotdtl <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore_plotdtl", 
                          multiple=TRUE,
                          core=FALSE, maxitems = 3, 
                          header = "Select health indicators")
  
  
})


# ----- Dimension selector -----------------------------------------------

output$focus_dimension_explore <- renderUI({

  focusDimension_selector("focus_dimension_explore", multiple=TRUE, maxitems = 5)
  
  
})

output$focus_dimension_explore_map <- renderUI({
  
  focusDimension_selector("focus_dimension_explore_map", multiple=TRUE, maxitems = 1)
  
  
})


# ----- Variable selector -----------------------------------------------



output$dataTableItems_explore <- renderUI({

  list(
    
    selectInput(inputId = "dataTableItems",
                "Select table content",
                choices = .rdata[['all_table_variables']]$table_vars,
                selected = .rdata[['focus_table_variables']],
                multiple=TRUE)
  )
  
})


# ----- Variable selector -----------------------------------------------


output$dataTableItemsSummary_explore <- renderUI({
  
  
  list(
    
    selectInput(inputId = "dataTableItemsSummary",
                "Select table content",
                choices = .rdata[['all_table_variables_summary']]$table_vars,
                selected = .rdata[['focus_table_variables_summary']],
                multiple=TRUE)
  )
  
})


# ----- Plot type -----------------------------------------------


output$disag_plot_type_explore<- renderUI({
  
  radioButtons("disag_plot_type_explore", "Select graph type",
               c("Bar graph" = "Bar",
                 "Line graph" = "Line"),
               inline=T,
               selected="Line")
  
  
})



output$disag_plot_type_compare<- renderUI({
  
  radioButtons("disag_plot_summary_pts", "Select graph style",
               c("Points" = "points",
                 "Labels" = "labels"),
               inline=T,
               selected="points")
  
  
})





# ----- Plot type -----------------------------------------------


output$disag_plot_mode_explore_dtl <- renderUI({
  
  radioButtons("disag_plot_mode_explore_dtl", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})

# ----- Plot type -----------------------------------------------

output$disag_plot_mode_explore <- renderUI({
  
  radioButtons("disag_plot_mode_explore", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})


# ----- Disaggregated error bars -----------------------------------------------

output$disag_plot_error_bars <- renderUI({
  checkboxInput('disag_error_bars', 'Include 95% confidence interval', FALSE)
})


# ----- Plot dimensions -----------------------------------------------


output$disag_plot_dimensions_explore <- renderUI({
  
  list(
    sliderInput('plot_height1', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width1', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})


# ----- Plot dimensions -----------------------------------------------

output$summary_plot_dimensions_explore <- renderUI({
  
  list(
    sliderInput('plot_height2', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width2', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})




output$disag_plot_dimensions_explore_dtl <- renderUI({
  
  list(
    sliderInput('plot_height_dtl', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width_dtl', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})

# ----- Plot dimensions -----------------------------------------------

output$summary_plot_dimensions_compare <- renderUI({
  
  list(
    sliderInput('plot_height4', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width4', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})


# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_table <- renderUI({
  selectInput("focus_inequal_type_explore_table", 
              "Select summary measures", 
              choices= .rdata[['summary_measures_all']], 
              selected=.rdata[["focus_inequal_type"]], 
              multiple=TRUE)
})

# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_plot <- renderUI({
  focusInequalType_selector("focus_inequal_type_explore_plot", multiple=FALSE)
})



# ----- Multiplier -----------------------------------------------

# No longer used
# output$summary_measures <- renderUI({
#   list(
#     tags$span(class="control-label", "Select estimate display"),
#     checkboxInput('summultiplier1', 'MLD and TI multiplied by 1000', TRUE),
#     checkboxInput('summultiplier2', 'RCI multiplied by 100', TRUE)#,
#     
#   )
# })


# ----- Download data-----------------------------------------------

output$downloadSummtable <- renderUI({ 
  theData <- datasetInequal()
  
  if(is.null(theData)){
    return()
  }
  if(nrow(theData)==0){
    return()
  } else {
    list(br(),
         actionButton("downloadSummtable", "Download data", class = "btn-primary"))
  }  
})



# ----- Summary plot type ---------------------------------------------

output$summary_plot_type_explore <- renderUI({
  radioButtons("summary_plot_type_explore", "Select graph type",
               c("Bar graph" = "Bar",
                 "Line graph" = "Line"),
               inline=T,
               selected="Bar")
})


output$summary_plot_mode_explore <- renderUI({
  
  radioButtons("summary_plot_mode_explore", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})

# ----- Summary error bars ---------------------------------------------

output$summary_plot_error_bars <- renderUI({
  checkboxInput('summary_error_bars', 'Include 95% confidence interval', FALSE)
})

# ----- Summary error bars ---------------------------------------------




output$summary_plot_CI_type <- renderUI({
  radioButtons("summary_CI_type", NA,
               c("Analytic CI" = "analytic",
                 "Bootstrap CI" = "bootstrap"),
               inline=T,
               selected="analytic")
})



#******************************************************************************
# Compare inquality: sidepanel -----
#******************************************************************************



output$disag_plot_mode_compare <- renderUI({
  
  radioButtons("disag_plot_mode_compare", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})


output$summary_plot_mode_compare <- renderUI({
  
  radioButtons("summary_plot_mode_compare", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})






output$focus_indicator_compare <- renderUI({
  
  #indic<-ifelse(is.null(.rdata[['focus_indicator']]), focus_indicator, .rdata[['focus_indicator']])
  
  
  selectInput("focus_indicator_compare", 
              "Select health indicator", 
              choices  = .rdata[['full_indicators']], 
              multiple = FALSE, 
              selected = .rdata[['focus_indicator']][1])
  
})


output$focus_source_year_compare <- renderUI({
  
  # Need to reorder
  
  datsource <- .rdata[['data_sources']]
  focsource <- .rdata[['focus_data_source']]
  indx <- match(.rdata[['focus_data_source']], .rdata[['data_sources']])
  datsource <- c(datsource[indx], datsource[-indx])
  
  list(
    selectInput("focus_data_source_compare", "Select data sources",
                 choices = datsource, #c("All", "DHS", "MICS"),
                 selected=focsource,
                multiple = TRUE),
    tags$label(class="control-label", "Select year"),
    checkboxInput('mostrecent_compare', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_compare",  
                      
                      selectInput(inputId="focus_year_compare", 
                                  label=NA, 
                                  choices=c( .rdata[['all_years']]), 
                                  multiple=FALSE, 
                                  selected=.rdata[['focus_year']][1])
    )
  )
})



output$focus_summeasure_compare_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type_compare", multiple = FALSE)
})


output$focus_dimension_compare <- renderUI({
  
  focus_dimen <- .rdata[['focus_dimension']][1]
  selectInput(inputId = "focus_dimension_compare",
              "Select inequality dimension",
              choices = unique(.rdata[['equity_dimensions']]),
              selected = focus_dimen,
              multiple=FALSE,
              selectize=TRUE)
  
})



output$benchmark_countries <- renderUI({
  
  countries <- .rdata[['benchmark_countries']]
  focus <-.rdata[['focus_country']]
  
  countries <- countries[!countries%in%focus]
  
  .rdata[['benchmark_countries']]<<-countries
  
  
  selectInput("benchmark_countries", 
              "Select comparison settings", 
              choices=countries, 
              selected=countries,
              multiple=TRUE)
})



output$benchmarkWBgroup <- renderUI({
  
  selectInput("benchmarkWBgroup", label = "Filter by country-income group",
              choices = .rdata[['income_groups']],
              selected = .rdata[['focus_income_group']],
              multiple=T)
})








output$benchmarkWHOregion <- renderUI({
  

  selectInput("benchmarkWHOregion", label = "Filter by WHO Region",
              choices=.rdata[['who_regions']],
              selected = .rdata[['focus_who_regions']],
              multiple=T)
  
})


output$benchmarkYears <- renderUI({
  list(
    sliderInput('benchmarkYears', 'Select years', min=0, max=5, value=2, step = 1,
                round = T, ticks = TRUE, animate = FALSE),
    helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
             style="color:#666666; font-size: 85%")
  )
  
})



output$disag_explore_map_container <- renderUI({
  list(
    div(id = "disag_explore_map_container"),
    tags$footer(tags$script(src = "create_map.js"))
  )

})

















