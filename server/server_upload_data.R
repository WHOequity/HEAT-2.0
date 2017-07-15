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




get_upload_data <- eventReactive(input$doUploadBtn, {
  
  res <- list(type = NULL, res = NULL)
  if(input$neworexisting != "newdata") return()
  
  
  if(is.null(input$filename)){
    res$type <- "message"
    res$res <- "Please choose a file."
    return(res)
  }
  
  
  inFile <- input$filename
  ext <- file_ext(inFile$name)
  
  
  if(!ext%in%c("xls", "xlsx", "csv", "txt")){
    res$type <- "message"
    res$res <- "This file does not appear to be a csv or Excel file, it must have a csv, txt, xls or xlsx extension."
    return(res)
  }
  
  
  
  if( ext %in% c("csv", "txt")){
    #cp1252
    dat <- suppressWarnings(try(read.csv(inFile$datapath, check.names = FALSE, 
                                         stringsAsFactors = FALSE), silent = TRUE))
    
    if("try-error"%in%class(dat)){
      res$type <- "message"
      res$res <- "There is a problem with your CSV. Perhaps the wrong format?"
      return(res)
    }
    
    if(nrow(dat)<2){
      res$type <- "message"
      res$res <- "Your CSV does not have enough data to use in HEAT."
      return(res)
    }
    
  }
  
  if(ext %in% c("xls", "xlsx")){
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    dat <- try(read_excel(paste(inFile$datapath, ext, sep="."), sheet = 1), 
               silent=TRUE)
    
    if("try-error"%in%class(dat)){
      res$type <- "message"
      res$res <- "There is a problem with your Excel file. Perhaps the wrong format or perhaps you specified the wrong sheet number?"
      return(res)
    }
    
    
    if(nrow(dat)<2){
      res$type <- "message"
      res$res <- "Your Excel file does not have enough data to use in HEAT. Perhaps the wrong format or perhaps you specified the wrong sheet number?"
      return(res)
    }
  }
  
  
  
  names(dat)[names(dat) == "setting"] <- "country"
  names(dat)[names(dat) == "setting_average"] <- "national"
  
  
  # These are tests I'm performing before 
  # doing some type converstions
  
  # Very first thing
  missing.required.variables <- missing_required_variables(dat)
  
  # If we're missing columns
  if(missing.required.variables$result){
    res$type <- "message"
    res$res <- missing.required.variables$msg
    return(res)
  }
  
  # Before converting integers to integers test
  # that they are, in fact, integers
  not.discrete <- not_discrete(dat)
  if(not.discrete$result){
    res$type <- "message"
    res$res <- not.discrete$msg
    return(res)
  }
  
  
  # We need to test if there are strings in what should be
  # numeric columns. This addition may lead to redundancy
  
  
  # If not do some quick data fixes
  #dat <- uploaded_data_adjustments(dat)
  
  
  some.strings.or.NotEmpty  <- some_strings_or_NotEmpty (dat)
  
  if(some.strings.or.NotEmpty$result){
    res$type <- "message"
    res$res <- some.strings.or.NotEmpty$msg
    return(res)
  }
  
  # Now apply the tests
  msg <- data_upload_test(dat)
  

  
  
  
  if(grepl("Data not uploaded", msg)){
    res$type <- "message"
    res$res <- msg
  } else {
    res$type <- "data"
    res$res <- dat
  }
  
  res
})






output$table_result <- renderUI({
  
  input$doUploadBtn
  
  create_new_data <- isolate(input$neworexisting) == "newdata"
  
  
  # if they choose new data
  if(create_new_data){
    
    valid_name <- grepl("^[0-9a-zA-Z ..._-]+$", isolate(input$datafoldername))
    if(!valid_name) return(list(tags$div(class="datawarning", 
                                           "Your folder name cannot include non-word characters.")))
    
    upload.data <- get_upload_data()
    
    
    if(upload.data$type == "message"){
      session$sendCustomMessage(type = "resetFileInputHandler", "filename")
      return(list(tags$div(class="datawarning", upload.data$res)))
    }
    
    # this returns the strata and maindata
    res <- create_raw_tables(upload.data$res)

    
    inequals <- lapply_inequals(res$strata, res$maindata)

 
    # Git 868, now that we don't need r_national or boot.se they are
    # all SE and end up as a logical and we get an error when we
    # round
    
    inequals <- mutate(inequals,
      boot.se  = as.numeric(boot.se),
      r_national = as.numeric(r_national)
    )


    # We've decided (per GitHub 480) that for all except mdb we 
    # we should use analytic SE

    inequals$final_se <- inequals$se
    inequals$final_se[inequals$measure%in%c("mdb", "mdm", "idis")] <- 
      inequals$boot.se[inequals$measure%in%c("mdb", "mdm", "idis")]
    
    #outpath <- paste0("./data/user-data/", isolate(input$datafoldername), "/")
    outpath <- "./data/user-data/"
    
    change_names_preserve_data(inequals = inequals, maindata = res$maindata, strata = res$strata,
                               savedata = TRUE, outpath = outpath, prefix =isolate(input$datafoldername) )
    
    
    updateSelectInput(session, "selectfolders", choices = get_data_folders())
    
    
  }else{
    
    selectfolders <- paste0(isolate(input$selectfolders), "____")
    
    if(selectfolders == "none") return(list(tags$div(class="datawarning", "There is no existing data please upload new data")))
    existdatapath <- paste0("data/user-data/", selectfolders)
    thefiles <- list.files("data/user-data/")
    thefiles <- thefiles[grepl(selectfolders, thefiles)]
    thefiles <- gsub(selectfolders, "",  thefiles, fixed = TRUE)
    
    hasallfiles <- all(tolower(c("inequals.RDS", "maindata.RDS", "strata.RDS", 
                                 "dimensions.RDS", "years.RDS"))%in%tolower(thefiles))
    if(!hasallfiles) return(list(tags$div(class="datawarning", "This folder does not have all the required files")))
    
    use_existing_data(existdatapath)
    
    
  }
  
  
  # this fills in the 
  fill_in_global_list()

  # this works but disabled for now
  updateSelectInput(session, "focus_country_explore", choices = .rdata[['all_countries']],
                    selected = .rdata[['focus_country']])
  
  updateSelectInput(session, "focus_dimension_explore", choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']])
  
  updateSelectInput(session, "focus_indicator_explore", choices = .rdata[['full_indicators']],
                    selected = .rdata[['focus_indicator']])
  
  updateSelectInput(session, "focus_data_source_explore", choices = .rdata[['data_sources']],
                    selected = .rdata[['focus_data_source']])
  
  updateSelectInput(session, "focus_year_explore", choices = .rdata[['all_years']],
                    selected = .rdata[['focus_year']])
  
  updateSelectInput(session, "focus_country_compare", choices = .rdata[['all_countries']],
                    selected = .rdata[['focus_country']])
  
    
  updateSelectInput(session, "focus_indicator_compare", choices = .rdata[['full_indicators']],
                    selected = .rdata[['focus_indicator']])
  
  updateSelectInput(session, "focus_dimension_compare", choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']])

  updateSelectInput(session, "focus_data_source_compare", choices = .rdata[['data_sources']],
                     selected = .rdata[['focus_data_source']])
  
  updateSelectInput(session, "focus_year_compare", choices = .rdata[['all_years']],
                    selected = .rdata[['focus_year']])
  
  
  updateSelectInput(session, "benchmarkWBgroup",              
                    choices = .rdata[['income_groups']],
                    selected = .rdata[['focus_income_group']])
  
  updateSelectInput(session, "benchmarkWHOregion",               
                    choices=.rdata[['who_regions']],
                    selected = .rdata[['focus_who_regions']])
  
  
  updateTextInput(session, "main_title1", value = .rdata[["plotDisag_explore_title"]])
  updateTextInput(session, "main_title2", value = .rdata[["plotSummary_explore_title"]])
  updateTextInput(session, "main_title3", value = .rdata[["plotDisag_compare_title"]])
  updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
  updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
  
  
  
  updateSelectInput(session, "benchmark_countries", 
                    choices=.rdata[['benchmark_countries']], 
                    selected=.rdata[['benchmark_countries']])
  
  outpath <- paste0("./data/user-data/", isolate(input$datafoldername))
  
  if(create_new_data){
    session$sendCustomMessage(type = "resetFileInputHandler", "filename")
    return(HTML("<div class='datasuccess'>
		<span class='msg_upload'>Data imported successfully!</span><br>
		<span>Click on Explore Inequality in the blue band at the top to explore the situation in one setting 
		(e.g. your country, province, or district of interest). Then go on and click on Compare Inequality to compare the situation in one setting with the situation in other settings.</span></div>"))
  }
 
 
 # if(create_new_data){
 #   session$sendCustomMessage(type = "resetFileInputHandler", "filename")
 #   return(list(tags$div(class="datawarning", 
 #                        paste("Data imported successfully! Data stored in folder", outpath))))
 # }
  
  
  
  if(!create_new_data){
     msg <- paste("<div class='datasuccess'><span class='msg_upload'>Data opened successfully!</span><br> Click on Explore Inequality in the blue band at the top to explore the situation in one setting (e.g. your country, province, or district of interest). Then go on and click on Compare Inequality to compare the situation in one setting with the situation in other settings.</div>")
    return(HTML(msg))
  }
  
  
})

