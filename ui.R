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

#devtools::install_github('jbkunst/highcharter')
#library(highcharter)
library(shinyBS)

.rdata <<- list()
.rdata[['save_objects']] <<- ls(envir = globalenv(), all.names = TRUE)
# This piece allows us to specify the version and then load associated
# landing page and CSS file
.rdata[['HEATversion']] <<- "whodata" # "whodata" or "upload"
.rdata[['landing_page']] <<- paste0("www/landing_page_",.rdata[['HEATversion']], ".html")
.rdata[['extra_css']] <<- paste0("style_",.rdata[['HEATversion']], ".css")


source("ui/ui_widgets_landing_page.R", local=TRUE)

# Originally this was broken into clean separate files but I found
# that loading these took longer than having them all in this one
# super-long UI
shinyUI(
  
  tagList(
    tags$script(HTML('function resetFormElement(e) {
  e.wrap("<form>").closest("form").get(0).reset();
  e.unwrap();

  // Prevent form submission
  //e.stopPropagation();
  //e.preventDefault();
}')),
    tags$script('

        Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
          $("#filename").parent()[0].reset();
          $("#filenametxt").val("No file selected");


        //var control = $("#filename");
        //control.replaceWith( control = control.clone( true ) );
        console.log("blah");


        });
      '),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="spacelab.min.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="style.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css",href= .rdata[['extra_css']])),
    tags$head(tags$link(rel="stylesheet", type="text/css",href= 'style_hc.css')),
    tags$head(tags$script(src = "script.js")),
    #tags$head(tags$script(src = "indonesia_subnational_boundaries01.js")),
    #tags$head(tags$script(src = "mapdata.js")),
    
    tags$head(tags$title("Health Equity Assessment Toolkit")),
    
    # UNCOMMENT HERE TO RESTORE MODAL
         tags$head(HTML('<div id="myModal" class="modal fade">
         <div class="modal-dialog">
             <div class="modal-content">
                 <div class="modal-header">
                     <h4 class="modal-title">Terms of use and software license agreement</h4>
                 </div>
                 <div class="modal-body" >
                 <div id="modal-license">
     
                     </div>
                     <button  class="btn btn-primary" data-dismiss="modal">I accept</button>
                 </div>
             </div>
         </div>
     </div>')),
    
    # The JS below is designed to activate the modal and
    # to change the navbar if the upload version is being
    # used (remove logo and and add "Plus")
    
    tags$head(HTML("<script type='text/javascript'>
                      $( document ).ready(function() {

      $('#myModal').modal('show');
      $('#modal-license').load('license_agreement.html');
      if($('#backimg-upload').length){
      $('.whoimg').remove();
      $('#filename_progress').remove();
      $('.dropdown').addClass('pull-right');
      $('.navtext').text($('.navtext').text().concat(' Plus'))
      }
                      });
                      </script>")),
    
    navbarPage(title =  HTML('<span class="navtitle"><a rel="home" href="#" title="World Health Organization"><img class="whoimg" src="who_logo_white40px.png"></a><span class="navtext">Health Equity Assessment Toolkit</span></span>'),
               id= "who_heat", 
               inverse=TRUE, 
               collapsible = TRUE,
               tabPanel("Home", htmlTemplate(filename = .rdata[['landing_page']],
                                             ui_landing_uploadnew = ui_landing_uploadnew,
                                             ui_landing_foldersel = ui_landing_foldersel,
                                             ui_landing_neworexisting = ui_landing_neworexisting,
                                             busyindicator = busyIndicator())),
               #tabPanel("Home", includeHTML('www/landing_page_upload.html')),
               tabPanel("Explore Inequality", #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
                        
                        # EXPLORE INEQUALITY UI, ORIGINALLY I USED RENDERUI BUT THIS WAS DEFINITELY SLOWER
                        
                        sidebarLayout(
                          sidebarPanel(
                            tags$div(class="sectionhead1", "Explore inequality"),
                            uiOutput("focus_country_explore"), 
                            
                            conditionalPanel(condition = "!(input.assessment_panel == 'datamap' | input.assessment_panel == 'dataplot_dtl')",
                                             uiOutput('focus_source_year_explore'),
                                             uiOutput("focus_indicator_explore"),
                                             uiOutput("focus_dimension_explore")
                                             
                            ),
                            
                            conditionalPanel(condition = "(input.assessment_panel == 'datamap' | input.assessment_panel == 'dataplot_dtl')",
                                             uiOutput('focus_source_year_explore_map')
                                             
                            ),
                            
                            
                            # In the detailed graph tab the rules are similar to the map
                            # but for indicator we are allowing 3 rather than 1
                            conditionalPanel(condition = "input.assessment_panel == 'dataplot_dtl'",
                                             uiOutput("focus_indicator_explore_plotdtl")
                                             
                            ),
                            
                            conditionalPanel(condition = "(input.assessment_panel == 'datamap' | input.assessment_panel == 'dataplot_dtl')",
                                             uiOutput("focus_dimension_explore_map")
                                             
                            ),
                            
                            conditionalPanel(condition = "input.assessment_panel == 'dataplot_dtl'",
                                             #uiOutput("disag_plot_mode_explore_dtl"),
                                             uiOutput("disag_plot_explore_dtl_btn")
                                             
                                             
                            ),
                            
                            conditionalPanel(condition = "input.assessment_panel == 'dataplot'",  
                                             
                                             uiOutput("disag_plot_type_explore"),
                                             #uiOutput("disag_plot_mode_explore"),
                                             uiOutput("ui_collapse_explore_disag_plot")
                                             
                            ),
                            conditionalPanel(condition = "input.assessment_panel == 'datatable'",   #### output.owndata gopt from server.R
                                             
                                             uiOutput("ui_collapse_explore_disag_data")       
                                             
                            ),
                            
                            conditionalPanel(condition = "input.assessment_panel == 'sumplot'",
                                             
                                             uiOutput("focus_summeasure_explore_summary_plot"),
                                             uiOutput("summary_plot_type_explore"),
                                             #uiOutput("summary_plot_mode_explore"),
                                             uiOutput("ui_collapse_explore_sum_plot")
                                             
                            ),
                            conditionalPanel(condition = "input.assessment_panel == 'sumtable'",
                                             
                                             uiOutput("focus_summeasure_explore_summary_table"),
                                             uiOutput("ui_collapse_explore_sum_data")
                                             
                            )
                            
                          ),# end sidebarpanel
                          
                          
                          mainPanel(
                            bsModal("hc_model_explore", "", NULL, size = "large", 
                                    ""),
                            bsModal_alt(id="datatableModal_explore",
                                        title = "Download data",
                                        trigger = "btnDownloadDisagData_explore",
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
                         These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype1", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadDisagData_explore', label = "Start")),
                            
                            
                            
                            bsModal_alt(id = "dataplotModal_explore", title = "Download graph", trigger = "btnDownloadDisagPlot_explore", 
                                        #tags$p("Set the dimensions for the plot here and download it."),
                                        #br(),
                                        tags$p("Titles and axis labels are displayed according to your selections."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        #textInput("disagPlotWitdth_explore", "Graph width (cm)", value="24" ),
                                        #textInput("disagPlotHeight_explore", "Graph height (cm)", value="24" ),
                                        selectInput(inputId="disagPlotType_explore", label='Output format:',
                                                    choices=c("PDF" = "PDF",
                                                              "PNG" = "PNG",
                                                              "JPG" = "JPG")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlot_explore', label = "Start")),
                            bsModal_alt(id = "dataplotModal_explore_dtl", title = "Download graph", trigger = "btnDownloadDisagPlot_explore_dtl", 
                                        #tags$p("Set the dimensions for the plot here and download it."),
                                        #br(),
                                        tags$p("Titles and axis labels are displayed according to your selections."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        #textInput("disagPlotWitdth_explore", "Graph width (cm)", value="24" ),
                                        #textInput("disagPlotHeight_explore", "Graph height (cm)", value="24" ),
                                        selectInput(inputId="disagPlotType_explore_dtl", label='Output format:',
                                                    choices=c("PDF" = "PDF",
                                                              "PNG" = "PNG",
                                                              "JPG" = "JPG")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlot_explore_dtl', label = "Start")),
                            
                            bsModal_alt(id = "compdataDisagModal_explore", title = "Download data", trigger = "btnDownloadDisagPlotData_explore", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_explore_disag", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlotData_explore', label = "Start")),
                            
                            bsModal_alt(id = "compdataDisagModal_explore_dtl", title = "Download data", trigger = "btnDownloadDisagPlotData_explore_dtl", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_explore_disag_dtl", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlotData_explore_dtl', label = "Start")),
                            
                            
                            
                            bsModal_alt(id = "summtableModal_explore", title = "Download data", trigger = "btnDownloadSummaryData_explore", 
                                        tags$p("The summary measures in the table will be downloaded as a text file with the values
                                  separated by a comma or a tab.  Select your preferred field separator and then download
                                  the data.  These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype2", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadSummaryData_explore', label = "Start")),
                            
                            bsModal_alt(id = "summplotModal_explore", title = "Download graph", trigger = "btnDownloadSummaryPlot_explore", 
                                        #tags$p("Set the dimensions for the plot here and download it."),
                                        # br(),
                                        tags$p("Titles and axis labels are displayed according to your selections."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        #textInput("summaryPlotWitdth_explore", "Graph width (cm)", value="24" ),
                                        #textInput("summaryPlotHeight_explore", "Graph height (cm)", value="24" ),
                                        selectInput(inputId="summaryPlotType_explore", label='Output format:',
                                                    choices=c("PDF" = "PDF",
                                                              "PNG" = "PNG",
                                                              "JPG" = "JPG")),
                                        downloadButton(outputId = 'btnStartDownloadSummaryPlot_explore', label = "Start")),
                            
                            bsModal_alt(id = "compdataSummaryModal_explore", title = "Download data", trigger = "btnDownloadSummaryPlotData_explore", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_explore_summary", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadSummaryPlotData_explore', label = "Start")),
                            
                            
                            
                            tabsetPanel(id="assessment_panel",
                                        
                                        tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='dataplot' ,
                                                 busyIndicator(),
                                                 uiOutput('btnDownloadDisagPlotData_explore'),
                                                 htmlOutput('disag_plot_explore')
                                        ), 
                                        tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(detailed bar graphs)<h6>"), value='dataplot_dtl' ,
                                                 busyIndicator(),
                                                 uiOutput('btnDownloadDisagPlotData_explore_dtl'),
                                                 htmlOutput("disag_plot_explore_dtl")
                                        ), 
                                        tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), value='datatable' , 
                                                 busyIndicator(),
                                                 uiOutput('btnDownloadDisagData_explore'),
                                                 uiOutput('dataTable')
                                        ), 
                                        # tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(maps)<h6>"), value='datamap' , 
                                        #          uiOutput("disag_explore_map_container")
                                        # ),
                                        
                                        tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='sumplot' ,
                                                 
                                                 busyIndicator(),
                                                 uiOutput('btnDownloadSummaryPlotData_explore'),
                                                 htmlOutput('summary_plot_explore')
                                        ),
                                        
                                        tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), value='sumtable' , 
                                                 
                                                 
                                                 busyIndicator(),
                                                 
                                                 uiOutput('btnDownloadSummaryData_explore'),
                                                 uiOutput(outputId="dataTableInequal")
                                        )              
                                        
                            )#end tabsetPanel
                            
                          )#end mainPanel
                        )
               ),
               tabPanel("Compare Inequality", 
                        
                        # COMPARE INEQUALITY UI, ORIGINALLY I USED RENDERUI BUT THIS WAS DEFINITELY SLOWER
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            tags$div(class="sectionhead1", "Compare inequality"),
                            uiOutput("focus_country_compare"),
                            uiOutput('focus_source_year_compare'),
                            #uiOutput("focus_year_compare"),
                            uiOutput("focus_indicator_compare"),
                            uiOutput("focus_dimension_compare"),
                            
                            conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                                             uiOutput("focus_summeasure_compare_summary")),
                            tags$div(class="sectionhead", "Benchmark options"),
                            uiOutput("benchmarkWBgroup"),
                            uiOutput("benchmarkWHOregion"),
                            uiOutput("benchmark_countries"),
                            uiOutput('benchmarkYears'),
                            
                            
                            
                            conditionalPanel(condition = "input.comparison_panel == 'inequaldisag'",
                                             
                                             #uiOutput("disag_plot_mode_compare"),
                                             uiOutput("ui_collapse_compare_disag_plot")
                                             
                            ),
                            conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                                             
                                             #uiOutput("summary_plot_mode_compare"),
                                             uiOutput("disag_plot_type_compare"),
                                             uiOutput("ui_collapse_compare_sum_plot")       
                                             
                            )
                            
                            
                          ),# end sidebarpanel
                          
                          
                          mainPanel(
                            bsModal("hc_model_compare", "", NULL, size = "large", 
                                    ""),
                            bsModal_alt(id = "compdataModal_compare", title = "Download data", trigger = "btnDownloadDisagData_compare", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
               These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_benchmark", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadDisagData_compare', label = "Start")),
                            bsModal_alt(id = "compplot1Modal_compare", title = "Download graph", trigger = "btnDownloadDisagPlot_compare", 
                                        #tags$p("Set the dimensions for the plot here and download it. "),
                                        # br(),
                                        tags$p("Titles and axis labels are displayed according to your selections."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        #textInput("disagPlotWitdth_compare", "Graph width (cm)", value="24" ),
                                        #textInput("disagPlotHeight_compare", "Graph height (cm)", value="24" ),
                                        selectInput(inputId="disagPlotType_compare", label='Output format:',
                                                    choices=c("PDF" = "PDF",
                                                              "PNG" = "PNG",
                                                              "JPG" = "JPG")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlot_compare', label = "Start", class = NULL)),
                            bsModal_alt(id = "compdataDisagModal_compare", title = "Download data", trigger = "btnDownloadDisagPlotData_compare", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_benchmark_disag", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadDisagPlotData_compare', label = "Start")),
                            bsModal_alt(id = "compplot2Modal_compare", title = "Download graph", trigger = "btnDownloadSummaryPlot_compare", 
                                        #tags$p("Set the dimensions for the plot here and download it."),
                                        #br(),
                                        tags$p("Titles and axis labels are displayed according to your selections."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        #textInput("summaryPlotWitdth_compare", "Graph width (cm)", value="24" ),
                                        #textInput("summaryPlotHeight_compare", "Graph height (cm)", value="24" ),
                                        selectInput(inputId="summaryPlotType_compare", label='Output format:',
                                                    choices=c("PDF" = "PDF",
                                                              "PNG" = "PNG",
                                                              "JPG" = "JPG")),
                                        downloadButton(outputId = 'btnStartDownloadSummaryPlot_compare', label = "Start", class = NULL)),
                            bsModal_alt(id = "compdataSummaryModal_compare", title = "Download data", trigger = "btnDownloadSummaryPlotData_compare", 
                                        tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                                        br(),
                                        tags$p("Close the window once the download has commenced."),
                                        br(),
                                        radioButtons(inputId="filetype_benchmark_summary", label='Field separator:',
                                                     choices=c("Comma separated valued" = "csv",
                                                               "Tab separated values" = "tsv")),
                                        downloadButton(outputId = 'btnStartDownloadSummaryPlotData_compare', label = "Start")),
                            tabsetPanel(id = "comparison_panel", 
                                        
                                        tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='inequaldisag', 
                                                 busyIndicator(),
                                                 uiOutput("btnDownloadDisagPlotData_compare"),
                                                 htmlOutput('disag_plot_compare')
                                        ),
                                        tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='inequalsum', 
                                                 busyIndicator(),
                                                 uiOutput('btnDownloadSummaryPlotData_compare'),
                                                 conditionalPanel(condition = "input.summary_plot_mode_compare == 'ggplot'",
                                                                  checkboxInput(inputId='points_ccode', 'Show setting codes', value=FALSE)
                                                 ),
                                                 #                            uiOutput('btnDownloadSummaryPlotData_compare'),
                                                 #                            uiOutput('btnDownloadSummaryPlot_compare'),
                                                 uiOutput('summary_plot_compare')
                                                 #div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('summary_plot_compare'))
                                        )
                            )#endtabsetpanel
                            
                            
                          )# end mainPanel
                        )# end sidebarLayout
               ),
               
               navbarMenu_drop("About", drop = ifelse(.rdata[['HEATversion']]=="upload", 3, -1),
                               
                               tabPanel(h6("User manual"), value='gloss_panel', includeHTML("www/manual.html")),
                               tabPanel(h6("Technical notes"), value='gloss_panel', includeHTML("www/technical.html")),
                               tabPanel(h6("Indicator compendium"), value='gloss_panel', includeHTML("www/compendium.html")),
                               
                               tabPanel(h6("Software"), value='gloss_panel', includeHTML("www/software.html")),  
                               tabPanel(h6("Versions"), value='gloss_panel', includeHTML("www/versions.html")),
                               tabPanel(h6("License"), value='gloss_panel', includeHTML("www/license.html")),
                               tabPanel(h6("Feedback"), value='gloss_panel', includeHTML("www/feedback.html")),
                               tabPanel(h6("Acknowledgements"), value='gloss_panel', includeHTML("www/acknowledgement.html"))

                               
                               
               )
               
               
               
               
               
    ) # end navbarpage
  )  # End shinyUi
)




