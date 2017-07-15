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





uploaded_data_adjustments <- function(dat){
  
  # strip any rows that are entirely NA
  dat <- strip_all_NA_rows(dat)
  
  # see if any of these fields have any non-digits
  
  # Convert anything that is supposed to be a number to a number
  # TODO: right now this is hard-coded, perhaps add as global?
  
  integerVars <- c("year", "favourable_indicator", "ordered_dimension",
                   "subgroup_order", "reference_subgroup")  
  dat[,integerVars] <- suppressWarnings(lapply(dat[,integerVars], as.integer))
  
  numericVars <- c("estimate", "population", "se", "ci_lb", "ci_ub","national", "indicator_scale")
  dat[,numericVars] <- suppressWarnings(lapply(dat[,numericVars], as.numeric))
  
  charVars <- c("country", "source", "indicator_abbr", "indicator_name", "dimension",
                "subgroup", "flag", "iso3")
  dat[,charVars] <- suppressWarnings(lapply(dat[,charVars], trimws))
  
  dat
  


  
}


## ----- 1 Missing variables ----- 
missing_required_variables <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  reqname <-   vars <- c("country", "year", "source", "indicator_abbr", "indicator_name",
                         "indicator_scale", "dimension", "subgroup", "estimate", "se", "ci_lb",
                         "ci_ub", "population", "flag", "national", "iso3", "favourable_indicator",
                         "ordered_dimension", "subgroup_order", "reference_subgroup")
  missingReq <- reqname[!reqname%in%names(dat)]
  missingReq <- gsub("country", "setting", missingReq)
  missingReq <- gsub("national", "setting_average", missingReq)
  
  if(length(missingReq)>0){
    
    res$result <- TRUE
    #res$msg <- paste("Data not uploaded, you are missing required variables:",
    #                 paste(missingReq, collapse=", "))
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>You are missing the following variable(s):", paste(missingReq, collapse=", "), "</span")))
  }
  
  res
}



##  ----- 2 Missing values for mandatory variables (except "estimate") ----- 
missing_one_val <- function(dat){
  
  
  res <- list(result = FALSE, msg = NA)
  reqname <- c("country", "year", "source", "indicator_abbr", "indicator_name", "dimension", "subgroup", 
               "favourable_indicator", "indicator_scale", "ordered_dimension", 
               "subgroup_order", "reference_subgroup")
  
  thetest <- sapply(dat[,reqname], function(x) any(is.na(x) | x == ""))
  vals <- names(thetest)[thetest]
  vals <- gsub("country", "setting", vals)

  
  if(any(thetest)){
    res$result <- TRUE
    
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>You are missing values for the following mandatory variable(s):", paste(vals, collapse=", "), "</span")))
  }
  
  res
  
}

## 3 Missing values for mandatory variable "estimate"                     
empty_key_variables <- function(dat){
  res <- list(result = FALSE, msg = NA)
  #thetest <- sapply(strata, function(x) all(is.na(x$estimate) | x$estimate == ""))
  if(all(is.na(as.numeric(dat$estimate)))){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>You are missing values for the following mandatory variable: estimate.</span"))
  }
  
  res
  
}



## ----- 4 String variables are numeric -----
vars_not_char <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  charVars <- c("country", "source", "indicator_abbr", "indicator_name", "dimension",
                "subgroup", "flag", "iso3")
  

  
  # here we test if all characters are digits or if we have digit period digit
  thetest <- sapply(dat[,charVars], function(x) any(grepl("^\\d+$|^\\d+\\.\\d+$", x)))
  vals <- names(thetest)[thetest]
  vals <- gsub("country", "setting", vals)
  if(any(thetest)){
    res$result <- TRUE
    
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>Variables that are supposed to be characters are numbers:",
                     paste(vals, collapse=", "), "</span")))
  }
  res
}


## ----- 5 iso3 not 3 characters long -----
iso_not3 <- function(strata, var = "iso3"){

  isoinfo <- !sapply(strata, function(x) all(is.na(x$iso3) | proper_length(x$iso3, 3)))
  
  res <- list(result = FALSE, msg = NA)
  if(any(isoinfo)){

    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable must have 3 characters or be missing: iso3.</span>"))
  }
  res
}

## ----- 6 Numeric variables are text  -----
some_strings_or_NotEmpty <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  
  onlyNumber <- c("year", "estimate", "population", "se", "ci_lb", "ci_ub", "national", "favourable_indicator", "indicator_scale", "ordered_dimension", "subgroup_order", "reference_subgroup")
  
  thetest <- sapply(dat[,onlyNumber], function(x) all(grepl("[0-9]\\d*(\\.\\d+)?$",x) | is.na(x)))
  
  if(!all(thetest)){
    vars <- names(thetest)[!thetest]
    vars <- gsub("national", "setting_average", vars)
    
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>Variables that are supposed to be numbers contain non-numeric values:", paste0(vars, collapse=", "),
                                   "</span")))
  }
  res
  
  
}




## ----- 7 Year is not 4 digits long -----
year_test <- function(dat, var = "year"){
  res <- list(result = FALSE, msg = NA)
  
  var <- dat[[var]]
  
  thetest <- is_numeric(var, test_all = TRUE) & proper_length(var, 4)
  
  if(!thetest){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable must have four digits: year.</span>"))
  }
  res
}



## ----- 8 Numeric variables are not integers -----
not_discrete <- function(dat, vars = c("year","favourable_indicator", "indicator_scale", 
                                       "ordered_dimension", "subgroup_order","reference_subgroup" )){
  
  res <- list(result = FALSE, msg = NA)
  thetest <- which(sapply(dat[,vars], function(x) any(as.numeric(x)%%1!=0)))
  
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable(s) must be integers:", paste0(names(thetest), collapse=", "),
                                   "</span")))
  }
  res
  
}


## ----- 9 Numeric variables are negative -----
has_negative <- function(dat, vars = c("year", "estimate", "se", 
                                       "ci_lb", "ci_ub", "population", "national",
                                       "favourable_indicator", "indicator_scale", 
                                       "ordered_dimension", "subgroup_order","reference_subgroup")){
  
  res <- list(result = FALSE, msg = NA)
  
  thetest <- which(sapply(dat[,vars], function(x) any(x<0)))
  
  vals <- names(thetest)
  vals <- gsub("national", "setting_average", vals)
  
  
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable(s) cannot contain negative numbers:", paste0(vals, collapse=", "),
                     "</span>")))
  }
  res
  
}




## ----- 10 Population is 0 -----
pop_zero <- function(dat){
    res <- list(result = FALSE, msg = NA)
  thetest <- any(!is.na(dat$population) && dat$population <=0)

  if(thetest){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable must be missing or be greater than 0: population.</span>"))
  }
  res
}

## ----- 11 Favourable_indicator is not 0 or 1 (not: add a separate test 12 and 15 for ordered_dimension and reference_subgroup) -----
not_01_fav <- function(dat, vars = c("favourable_indicator" 
)){
  res <- list(result = FALSE, msg = NA)
  
  thetest <- which(sapply(dat[,vars], function(x) any(!grepl("^1$|^0$", x))))
  
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable contains values that are not 0 or 1: favourable_indicator. Favourable_indicators must be 1 for favourable indicators and 0 for adverse indicators.</span>")))
  }
  res
  
  
}


##12 Ordered_dimension is not 0 or 1
not_01_ordered <- function(dat, vars = c("ordered_dimension")){
  res <- list(result = FALSE, msg = NA)
  thetest <- which(sapply(dat[,vars], function(x) any(!grepl("^1$|^0$", x))))
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable contains values that are not 0 or 1: ordered_dimension. Ordered_dimension must be 1 for ordered dimensions and 0 for non-ordered dimensions." , "</span>")))
  }
  res
}



## ----- 13 Subgroup order is not 0 when ordered_dimension = 0 OR subgroup_order is 0 when ordered_dimension = 1 -----
subgroup_match_ordered <- function(strata){
  
  res <- list(result = FALSE, msg = NA)
  
  # her we're saying that if it is NOT an ordered dimension
  # then the subgroup_order needs to be 0. Otherwise the
  # subgroup_order needs to be NOT 0
  
  thetest <- any(sapply(strata, function(x){
    
    ordval <- unique(x$ordered_dimension)
    if(ordval == 0){
      return(!all(x$subgroup_order == 0))
    }
    if(ordval == 1){
      return(!all(x$subgroup_order!=0))
    }
    
    
  })
  )

  if(thetest){
    res$result <- TRUE

    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable does not correspond with the variable ordered_dimension: subgroup_order.</span><br><span>For ordered dimensions (ordered_dimension = 1), subgroup_order must be an increasing sequence of numbers starting with 1 for the most-disadvantaged subgroup. For non-ordered dimensions (ordered_dimension = 0), subgroup_order must be 0 for all subgroups. </span>"))

  }
  res
}


## ----- 14 Subgroup_order is not an increasing sequence when ordered_dimension = 1 -----
# if an ordered dimension then the sequence must be
# 1:nrow(strata)
sequence_if_ordered <- function(strata){
  res <- list(result = FALSE, msg = NA)
  
  thetest <- sapply(strata, function(x){
    #x<- strata[[1]]
    if(any(x$ordered_dimension != 1)) return(FALSE)
    ord <- as.integer(sort(x$subgroup_order))
    proper_ord <- seq(1, nrow(x))
    !identical(ord, proper_ord)
  })
  
  thetest <- names(thetest)[thetest]
  
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste0("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable does not correspond with the variable ordered_dimension: subgroup_order.</span><br><span>For ordered dimensions (ordered_dimension = 1), subgroup_order must be an increasing sequence of numbers starting with 1 for the most-disadvantaged subgroup.</span><br><span>Check the following strata: </span>", paste(thetest, collapse=", "))))
  }
  res
}

##15 Reference_subgroup is not 0 or 1
not_01_ref<- function(dat, vars = c("reference_subgroup")){
  res <- list(result = FALSE, msg = NA)
  thetest <- which(sapply(dat[,vars], function(x) any(!grepl("^1$|^0$", x))))
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable contains values that are not 0 or 1: reference_subgroup. For ordered dimensions (ordered_dimension = 1), reference_subgroup must be 0 for all subgroups</span><br><span>. For non-ordered dimensions (ordered_dimension = 0), reference_subgroup may be 1 for one subgroup (for the reference group only).</span>")))
  }
  res
}


## ----- 16 reference_subgroup is not 0 when ordered_dimension = 1 -----
# if they give a reference group AND an ordered dimension
ordered_and_refgrp <- function(strata){
  res <- list(result = FALSE, msg = NA)
  
  thetest <- sapply(strata, function(x){
    is_ordered(x) & ref_grp_exists(x)
  })
  
  thetest <- names(thetest)[thetest]
  
  if(length(thetest)!=0){
    res$result <- TRUE
    res$msg <- tags$div(HTML(paste0("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable does not correspond with the variable ordered_dimension: reference_subgroup.</span><br><span>For ordered dimensions (ordered_dimension = 1), reference_subgroup must be 0 for all subgroups.</span><br><span>Check the following strata: </span>", paste(thetest, collapse=", "))))
  }
  res
}


## ----- 17 reference_subgroup is 1 for more than one subgroup when ordered_dimension = 0 -----
strata_refsub_above1 <- function(strata){
  res <- list(result = FALSE, msg = NA)
  
  thetest <- any(sapply(strata, function(x) sum(x$reference_subgroup, na.rm=T))>1)
  
  if(thetest){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable is 1 for more than one subgroup within one strata: reference subgroup.</span><br><span>For non-ordered dimensions (ordered_dimension = 0), reference_subgroup may be 1 for one subgroup (for the reference group only).</span>"))
  }
  res
}

## ----- 18 Variables are not the same for all observations with the same setting -----
country_iso3_nomatch <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  myunique <- unique(dat[,c("country", "iso3")])
  thetest <- any(c(table(myunique$country), table(myunique$iso3))>1)
  
  if(thetest){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>Make sure that there is only one setting/iso3 combination for each setting.</span>"))
  }
  res
}



## ----- 19 Variables are not the same for all observations with the same setting, year, source, indicator -----
indic_nameabbr_notsame <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  myunique <- unique(dat[,c("country", "year", "source",
                            "indicator_abbr", "indicator_name",
                            "national", "favourable_indicator", "indicator_scale")])
  a <- count(myunique, country, year, source, indicator_name)$n
  b <- count(myunique, country,year, source, indicator_abbr)$n
  thetest <- any(c(a, b)>1)
  
  if(thetest){
    res$result <- TRUE

    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable(s) must take the same value for all observations with the same setting, year, source and indicator combination: indicator_abbreviation, indicator_name, setting_average, favourable_indicator and indicator_scale.</span>"))

  }
  res
}



## ----- 20 Variables are not the same for all observations with the same setting, year, source, indicator, dimension combination -----
ordered_notsame <- function(dat){
  
  res <- list(result = FALSE, msg = NA)
  myunique <- unique(dat[,c("country", "year", "source",
                            "indicator_abbr", 
                            "dimension", "ordered_dimension")])
  thetest <- any(count(myunique, country, year, source, indicator_abbr, dimension)$n>1)
  
  if(thetest){
    res$result <- TRUE

    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable(s) must take the same value for all observations with the same setting, year, source, indicator and dimension combination: ordered_dimension.</span>"))

  }
  res
}





# NO NUMBER
# vars_not_all_numbers <- function(dat){
#   res <- list(result = FALSE, msg = NA)
#   myunique <- unique(dat[,c("country", "year", "source",
#                             "indicator_abbr", 
#                             "dimension", "ordered_dimension")])
#   thetest <- any(count(myunique, country, year, source, indicator_abbr, dimension)$n>1)
#   
#   if(thetest){
#     res$result <- TRUE
#     res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded</span><br><span>Within a country, year, source, indicator and dimension the ordered_dimension values need to be the same</span>"))
#   }
#   res
# }


# # NO NUMBER, git 810
scale_has_zero_orNA <- function(dat){
  res <- list(result = FALSE, msg = NA)
  #thetest <- sapply(strata, function(x) all(is.na(x$estimate) | x$estimate == ""))
  if(any(dat$indicator_scale == 0 | is.na(dat$indicator_scale))){
    res$result <- TRUE
    res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded.</span><br><span>The following variable must be greater than 0: indicator_scale.</span"))
  }
  
  res
  
}




data_upload_test <- function(dat){
  
  # -- test 1:
  # missing key variables is in server_upload_data
  # because it needs to be applied right away
  
  # -- test 2: key variables missing a value
  res <- missing_one_val(dat)
  if(res$result) return(res$msg)
  rm(res)
  
 
  # -- test 3:  key variables all missing
  res <- empty_key_variables(dat)
  if(res$result) return(res$msg)
  rm(res)
  

  # -- test 4: variables that should be character are number
  res <- vars_not_char(dat)
  if(res$result) return(res$msg)
  rm(res)
  
   strata <- split(dat, paste(dat$country, dat$year, dat$indicator_abbr, dat$dimension))
  
  # -- test 5: iso should be 3 digits
  res <- iso_not3(strata)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 6: some_strings_or_NotEmpty
  # in server_upload_data
  
  # -- test 7: year is length 4 and number
  res <- year_test(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 8: must be discrete
  # Note that I run this also in server_upload_data so
  # may not be necessary here.
  
  res <- not_discrete(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 9:  variables that need to be 0/1
  
  res <- has_negative(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 10: population is zero
  
  res <- pop_zero(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 11: variables that need to be 0/1, favourable_indicator
  
  res <- not_01_fav(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 12: variables that need to be 0/1, ordered
  
  res <- not_01_ordered(dat)
  if(res$result) return(res$msg)
  rm(res) 

  # -- test 13: The subgroup order needs to be 0 if the ordered_dimension = 0
  
  res <- subgroup_match_ordered(strata)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 14: Subgroup_order is not an increasing sequence when ordered_dimension = 1   
  
  res <- sequence_if_ordered(strata)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 15: Ordered_dimension is not 0 or 1
  
  res <- not_01_ref(dat)
  if(res$result) return(res$msg)
  rm(res) 
  
  
  # -- test 16: reference_subgroup is not 0 when ordered_dimension = 1 
  
  res <- ordered_and_refgrp(strata)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 17: ref subgroup > 1
  
  res <- strata_refsub_above1(strata)
  if(res$result) return(res$msg)
  rm(res)
  
  # -- test 18: unique country/iso3 combination
  
  res <- country_iso3_nomatch(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 19: indicator/indicator_abbr just one per country
  res <- indic_nameabbr_notsame(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  # -- test 20: ordered not same
  res <- ordered_notsame(dat)
  if(res$result) return(res$msg)
  rm(res)
  

  

  



  


  
  


  
  res <- scale_has_zero_orNA(dat)
  if(res$result) return(res$msg)
  rm(res)
  
  
  return("Tests are OK, upload proceeding")
}



strip_all_NA_rows <- function(dat){

  # dat <- dat2
  dat[dat==""] <- NA
  nas <- rowSums(is.na(dat))
  bad <- which(nas == ncol(dat))
  if(length(bad)>1) dat <- dat[-bad,]
  dat

}


proper_length <- function(var, l){
  n <- unique(nchar(var)) 
  if(length(n)!=1) return(FALSE)
  n == l
}






all_there <- function(var){
  length(var) == sum(!is.na(var))

}

is_numeric <- function(var, test_all = FALSE, test_notneg = FALSE){
  #var <- dat$national
  var <- suppressWarnings(as.numeric(var))
  if(all(is.na(var))) return(FALSE)
  if(test_all && !all_there(var)) return(FALSE)
  # !! careful, I apply this to the version with no NA
  if(test_notneg && any(var[!is.na(var)]<0)) return(FALSE)

  TRUE
}



