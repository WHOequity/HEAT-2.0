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

# *****************************************************************************
# EDIT AS NEEDED: Choose the initial settings here ----
# *****************************************************************************

focus_summary_measure <- "d"
numTitleChars <- 50
drop_summary_measure <- c() # e.g., c('riikm', 'mld')


.rdata[['the_plot']] <<- 1

new_to_old_names <- data.frame(old = c("country", "year",
     "source", "indic", "indic_name", "dimension", "subgroup", "r",
     "r_lower", "r_upper", "se", "pop", "flag", "r_national",
     "iso3", "maxoptimum", "indicator_scale", "rankable", "order",
             "reference_subgroup",
     "r_lower", "r_upper"),
     new = c("country", "year", "source", "indicator_abbr",
             "indicator_name", "dimension", "subgroup", "estimate",
             "ci_lb", "ci_ub","se", "population", "flag",
             "national", "iso3", "favourable_indicator",
             "indicator_scale",
             "ordered_dimension", "subgroup_order",
             "reference_subgroup",
             "95ci_lb",
             "95ci_ub"), stringsAsFactors = FALSE)




# *****************************************************************************
# Manual reactives
# *****************************************************************************


.trigger <- reactiveValues(
  focus_year_explore  = FALSE,
  focus_ind_explore   = FALSE,
  focus_dim_explore   = FALSE,
  focus_ineq_explore_table  = FALSE,
  focus_ineq_explore_plot  = FALSE,
  focus_year_compare  = FALSE,
  focus_ind_compare   = FALSE,
  focus_dim_compare   = FALSE,
  focus_year_explore_map = FALSE,
  focus_ind_explore_plotdtl = FALSE,
  focus_dim_explore_map = FALSE,
  focus_dtlsub_map      = FALSE,
  focus_sort_map       = FALSE,
  focus_ineq_compare = FALSE,
  focus_data_source_explore = FALSE,
  focus_data_source_compare = FALSE,
  focus_data_source_explore_map = FALSE,
  focus_benchmark_compare  = FALSE,
  focus_WHO_info = FALSE,
  runplot_disag_explore = FALSE,
  runplot_disag_detail = FALSE,
  runplot_summary_explore = FALSE,
  runplot_disag_compare = FALSE,
  runplot_summary_compare = FALSE,
  runtable_disag_explore = FALSE,
  runtable_summary_explore = FALSE,
  disag_plot_explore_title = FALSE,
  summary_plot_explore_title = FALSE,
  disag_plot_compare_title = FALSE,
  summary_plot_compare_title = FALSE,
  disag_plot_explore_dtl_title = FALSE
  
  #disag_plot_explore_justPlot = FALSE
  
)

debounce_ms <- 300
debounce_disag_explore <- debounce(.trigger$runplot_disag_explore, debounce_ms)
debounce_disag_explore_dtl <- debounce(.trigger$runplot_disag_detail, debounce_ms)
debounce_summary_explore <- debounce(.trigger$runplot_summary_explore, debounce_ms)

debounce_summary_compare <- debounce(.trigger$runplot_summary_compare, debounce_ms)

debounce_disag_explore_table <- debounce(.trigger$runtable_disag_explore, debounce_ms)
debounce_summary_explore_table <- debounce(.trigger$runtable_summary_explore, debounce_ms)

debounce_disag_compare_plot <- debounce(.trigger$runplot_disag_compare, debounce_ms)

# *****************************************************************************
# First time
# *****************************************************************************

.rdata[['first_time']] <<- TRUE
.rdata[['first_time_dtl']] <<- TRUE
.rdata[['first_time_summary_plot']] <<- TRUE
.rdata[['first_time_datasource_compare']] <<- TRUE
.rdata[['first_time_indicator_compare']] <<- TRUE
.rdata[['first_time_country_compare']] <<- TRUE
.rdata[['first_time_dimension_explore_map']] <<- TRUE
.rdata[['first_time_dimension_compare']] <<- TRUE
.rdata[['first_time_disag_table']] <<-TRUE 
.rdata[['first_time_summary_compare_plot']] <<- TRUE
.rdata[['first_time_disag_compare_plot']] <<- TRUE
.rdata[['first_time_benchmark']] <<-TRUE
.rdata[['first_time_benchmark_countries']] <<-TRUE


.rdata[['focus_inequal_type_previous']] <<- "d"


# *****************************************************************************
# In modal?
# *****************************************************************************

.rdata[['is_modal']] <<- FALSE

# *****************************************************************************
# Plot titles
# *****************************************************************************

.rdata[["plotDisag_explore_title"]] <<- "Health Equity Disaggregated"
.rdata[["plotSummary_explore_title"]] <<- "Health Equity Summary"
.rdata[["plotDisag_compare_title"]] <<- "Health Equity Disaggregated"
.rdata[["plotSummary_compare_title"]] <<-"Health Equity Summary"
.rdata[["plotDisag_explore_title_dtl"]] <<-"Health Equity Summary"


# *****************************************************************************
# MIGHT BE ABLE TO DELETE THIS
# *****************************************************************************

.rdata[['data_warning']] <<-"If estimates are not shown for a selected combination of variables, then data are not available."
table_options <<- NULL
.rdata[['table_options']] <<-list(pageLength = 100, dom='frtp', scrollX = TRUE, columnDefs = list(list(width = '400px', targets = c(3)))) 
#.rdata[['table_options']][['columnDefs']] <<- NULL




# *****************************************************************************
# Read in summary measures and order
# *****************************************************************************
summeasure <- readRDS("data/HEAT-data/summeasures.RDS")
summeasure$measure_abbr <- trimws(summeasure$measure_abbr)
summeasure$measure_name <- trimws(summeasure$measure_name)
summeasure <- arrange(summeasure, measure_name)

# ----- DROP SUMMARY MEASURES from above
summeasure <- filter(summeasure, !measure_abbr%in%drop_summary_measure)

summeasurevect <- summeasure$measure_abbr
names(summeasurevect) <- paste0(summeasure$measure_name, " (", summeasure$measure_abbr, ")")

.rdata[['summary_measures_all']] <<- summeasurevect
.rdata[['summary_measures_table']]<<-select(summeasure, measure_name, measure_abbr, logscale)




# *****************************************************************************
# Set table variables
# *****************************************************************************


.rdata[['all_table_variables']] <<- data.frame(table_vars = c("Setting", 
                                      "Year", 
                                      "Data source",
                                      "Health indicator abbreviation" ,
                                      "Health indicator name" ,
                                      "Inequality dimension" ,
                                      "Subgroup",
                                      "Estimate" ,
                                      "95%CI lower bound" ,
                                      "95%CI upper bound" ,
                                      "Population share %"   ,
                                      "Flag" ,
                                      "Setting average" 
                                      ), 
                                      var_type = c("text", "numeric", rep("text", 5),
                                                   rep("numeric", 4), "text", "numeric"),
                                      stringsAsFactors = FALSE)


.rdata[['focus_table_variables']]<<-c("Setting", 
                                     "Year",  
                                     "Health indicator name", 
                                     "Inequality dimension", 
                                     "Subgroup", 
                                     "Estimate", 
                                     "Population share %")


.rdata[['all_table_variables_summary']] <<- data.frame(table_vars = c("Setting", 
                                             "Year", 
                                             "Data source", 
                                             "Health indicator abbreviation",
                                             "Health indicator name", 
                                             "Inequality dimension", 
                                             "Summary measure abbreviation",
                                             "Summary measure name",
                                             "Estimate",
                                             "95%CI lower bound",
                                             "95%CI upper bound",
                                             "Setting average"),
                                             var_type = c("text", "numeric", rep("text", 6),
                                                          rep("numeric", 4)),
                                             stringsAsFactors = FALSE)




.rdata[['focus_table_variables_summary']]<<-c("Setting", 
                                             "Year",  
                                             "Health indicator name", 
                                             "Inequality dimension", 
                                             "Summary measure name",
                                             "Estimate")


.rdata[["centered_table_variables"]] <<- c("Health indicator abbreviation", 
                                          #"Summary measure abbreviation", 
                                          "Data source")




# *****************************************************************************
# Plot storage
# *****************************************************************************

.rdata[["disag_plot_explore"]] <<- NULL
.rdata[["summary_plot_explore"]] <<- NULL
.rdata[["disag_plot_compare"]] <<- NULL
.rdata[["summary_plot_compare"]] <<- NULL


# *****************************************************************************
# Number of characters in title
# *****************************************************************************


.rdata[["numTitleChars"]] <<- numTitleChars

# *****************************************************************************
# Blank plot for missing data
# *****************************************************************************
.rdata[["blank_plot"]] <<- rectGrob(gp = gpar(col=rgb(1,1,1,0)))


# *****************************************************************************
# Plot warnings
# *****************************************************************************

# .rdata[["explore_datamsg"]] <- "If estimates are not shown for a selected combination of variables, then data are not available."
# .rdata[["explore_nodatamsg"]] <- "There is no data for this combination of variables."
# 
# .rdata[["compare_datamsg"]] <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
# 
# .rdata[["logscalemsg"]] <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."


# *****************************************************************************
# Number of bootstrap iterations
# *****************************************************************************

.rdata[["bootstrap_iterations"]] <<- 200


# *****************************************************************************
# Plot foot notes
# *****************************************************************************

if(.rdata[['HEATversion']] == "whodata"){
txt1 <- "Source: Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing health inequalities in countries.\nBuilt-in database edition. Version 2.0. Geneva, World Health Organization, 2017."
txt2 <- "Data source: The disaggregated data used in this version were drawn from the WHO Health Equity Monitor database (2016 update)\nwhich may have been revised or updated since that time. The most recent version of that database is available on the WHO website."
}

if(.rdata[['HEATversion']] == "upload"){
txt1 <- "Health Equity Assessment Toolkit Plus (HEAT Plus): Software for exploring and comparing health inequalities in countries."
txt2 <- "Upload database edition. Version 1.0. Geneva, World Health Organization, 2017. WHO provides this toolkit without data,\nand all data added to, or resulting from, the toolkit are the sole responsibility of the user, not WHO."

}

.rdata[["plot_footnote"]] <<- textGrob( paste(txt1, txt2, sep="\n"),
                                            x=0.05,
                                            y=0.75,
                                            hjust=0,
                                       gp = gpar(fontface = "italic", 
                                                 fontsize = 9, 
                                                 col="grey50",
                                                 lineheight = 0.9))

# *****************************************************************************
# Table footnotes
# *****************************************************************************

if(.rdata[['HEATversion']] == "whodata"){
txt1 <- "Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing health inequalities in countries. Built-in database edition. Version 2.0. Geneva; World Health Organization; 2017."
txt2 <- "The disaggregated data used in this version were drawn from the WHO Health Equity Monitor database (2016 update) which may have been revised or updated since that time. The most recent version of that database is available on the WHO website."
txt3 <- "Disaggregated are available from: http://apps.who.int/gho/data/node.main.HE-1540?lang=en"
}

if(.rdata[['HEATversion']] == "upload"){
txt1 <- "Health Equity Assessment Toolkit Plus (HEAT Plus): Software for exploring and comparing health inequalities in countries."
txt2 <- "Upload database edition. Version 1.0. Geneva - World Health Organization - 2017. WHO provides this toolkit without data"
txt3 <- "and all data added to or resulting from the toolkit are the sole responsibility of the user not WHO."
}


.rdata[["table_footnote"]] <<- paste(txt1, txt2, txt3, sep="\n")

# *****************************************************************************
# inequal table rules
# *****************************************************************************

a <- data.frame(just2_subgroups = TRUE, ordered = FALSE,  measure = c("d", "r", "par", "paf"))
b <- data.frame(just2_subgroups = FALSE, ordered = FALSE, measure = c("d", "r", "par", "paf", "bgv", "idis","idisw", "mdb", "mdm", "mld", "ti"))
c <- data.frame(just2_subgroups = FALSE, ordered = TRUE, measure = c("d", "r", "par", "paf", "aci", "rci", "rii", "sii"))
d <- data.frame(just2_subgroups = TRUE, ordered = TRUE,  measure = c("d", "r", "par", "paf"))
.rdata[['inequal_rules_tmp']] <<- rbind(a, b, c, d)

