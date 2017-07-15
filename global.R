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




# We want the Health indicator name and Summary measure name variables to
# be wider columns. I could not find a way to do this with DT after 
# extensive attempts. Instead I forced them wide by adding non-breaking
# spaces. Originally I wrote the forceNonBreakFull function which basically
# would add a series of non-breaking spaces throughout the name with the
# occasional space that would allow the name to break. But this could result 
# in some odd situations where you either have very long or relatively short 
# breaks. So I re-wrote to add non-breaking spaces only in the first N chars.


forceNonBreak <- function (labels, maxCharPerLine = 14) 
{
  
  labels <- as.character(labels)
  n = length(labels)
  splitX = strsplit(labels, split=" ")
  #newLabels = rep("", n)
  newLabels = rep("", n)
  for (l in 1:n) {
    #l <- 1
    if(length(splitX[[l]]) == 1 | nchar(splitX[[l]][1])>maxCharPerLine){
      finline <- labels[l]
    }else{
      
      line <- splitX[[l]]
      charsums <- cumsum(nchar(line))
      
      if(all(charsums<=maxCharPerLine)){
        finline <- paste(line, collapse = "&nbsp;")
      }else{
        
        below <- charsums[charsums<=maxCharPerLine]
        belowMax <- max(below)
        indxMax <- which(charsums == belowMax)
        longline <- paste(line[1:indxMax], collapse = "&nbsp;")
        rest <- paste(line[(indxMax+1):length(line)], collapse=" ")
        finline <- paste(longline, rest, sep=" ")
        
      }
      
      
    }
    newLabels[l] = finline
  }
  newLabels
}


# Keep this function, see above for description
# forceNonBreakFull <- function (labels, maxCharPerLine = 14, split = " ", fixed = TRUE, 
#                           newsplit = split, keepSplitAtEOL = TRUE) 
# {
#   #labels <- rep("Births attended by skilled health personnel (in the two or three years preceding the survey) (%)", 3)
#   # split <- " "
#   # fixed = TRUE
#   # maxCharPerLine <- 30
#   # newsplit <- "&nbsp;"
#   labels <- as.character(labels)
#   n = length(labels)
#   splitX = strsplit(labels, split = split, fixed = fixed)
#   newLabels = rep("", n)
#   for (l in 1:n) {
#     #l <- 1
#     nl = ""
#     line = ""
#     if (nchar(labels[l]) > 0) 
#       for (s in 1:length(splitX[[l]])) {
#         #s <- 1
#         newLen = nchar(line) + nchar(splitX[[l]][s])
#         if (nchar(line) < 5 | newLen <= maxCharPerLine) {
#           nl = paste(nl, splitX[[l]][s], sep = newsplit)
#           line = paste(line, splitX[[l]][s], sep = newsplit)
#         }
#         else {
#           nl = paste(nl, splitX[[l]][s], sep = paste0(if (keepSplitAtEOL) 
#             newsplit
#             else "", " "))
#           line = splitX[[l]][s]
#         }
#       }
#     newLabels[l] = nl
#   }
#   substring(newLabels, nchar(newsplit) + 1)
# }
# 


# this is the function for writing the table as CSV or TSV
# with the header lines. I use file() first so that I can
# essentially write.csv and then read.csv to get the formatting
# right and then I add the footnote
# dat is the data, file is the path and sep is the separator

write_table_wcitation <- function(dat, file, sep, citation){
  
  tmp <- file(encoding = "cp1252")
  write.table(dat, tmp, sep=sep, fileEncoding = "cp1252", row.names = FALSE)
  dat <- readLines(tmp, encoding = "cp1252")
  tmp2 <- file(file, encoding = "cp1252")
  
  #if(.rdata[['HEATversion']] == "whodata"){
  cat(c(citation, dat), file=tmp2, sep="\n")
  #}else{
  #  cat(c(dat), file=tmp2, sep="\n")
  #}
  close(tmp)
  
  
  
}


# Function used to add the citation to the plots for download
# it is called in server_downloading. If it' the upload version
# then no citation is added

save_plot_wcitation <- function(file, theplot, citation, heights = c(0.9, 0.1), width = 24, height = 24){
  
  #if(.rdata[['HEATversion']] == "whodata"){
  g <- grid.arrange(theplot,  citation, ncol=1, nrow=2, heights=heights)
  #}else{
  #  g <- theplot
  #}
  
  ggsave(file, g, width=width, height=height, units="cm")
  
}



getISO3 <- function(countryname){
  filter(.rdata[['countryinfo']], country==countryname) %>% .$iso3
}



# originally using shinyBS but the modals were not working with
# the new version of Shiny (perhaps because of new bootstrap)
# This is my own code to create a function to create a modal 
# allowing for extra tags in the body after the paragraph

bsModal_alt <- function(id, title, trigger, ...){

  mo <- tags$div(class = "modal fade", id = id, 
                 `data-trigger` = trigger, 
                 tabindex = "-1",
                 role = "dialog",
                 tags$div(class="modal-dialog", role="document",
                          tags$div(class="modal-content",        
                                   tags$div(class = "modal-header", 
                                            tags$button(Type = "button", class = "close", `data-dismiss` = "modal",  HTML("&times;")), 
                                            tags$h3(title)), 
                                   
                                   tags$div(class = "modal-body"),
                                   tags$div(class = "modal-footer", 
                                            tags$a(href = "#", class = "btn btn-primary", 
                                                   `data-dismiss` = "modal", "Close")
                                   )# end modal footer
                          )#end modal content
                 )#end modal dialog
  )# end modal fade
  mo$children[[1]]$children[[1]]$children[[2]] <- 
    tagAppendChildren(mo$children[[1]]$children[[1]]$children[[2]], list = list(...))
  return(mo)
}



# bsModal_alt("myid", "mytrigger", "mytitle",
#             tags$p("This is a p"), tags$h3("this is h3"))



#http://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks-in-ggplot2
number_ticks <- function(n) {function(limits) pretty(limits, n)}

# from package (WGCNA)

splitLabels <- function(variable, value){
  
  return(formatLabels(value, maxCharPerLine = 15))
}




splitLabelsWide <- function(variable, value){
  
  return(formatLabels(value, maxCharPerLine = 25))
}



formatLabels <- function (labels, maxCharPerLine = 14, split = " ", fixed = TRUE, 
                          newsplit = split, keepSplitAtEOL = TRUE) 
{
  labels <- as.character(labels)
  n = length(labels)
  splitX = strsplit(labels, split = split, fixed = fixed)
  newLabels = rep("", n)
  for (l in 1:n) {
    nl = ""
    line = ""
    if (nchar(labels[l]) > 0) 
      for (s in 1:length(splitX[[l]])) {
        newLen = nchar(line) + nchar(splitX[[l]][s])
        if (nchar(line) < 5 | newLen <= maxCharPerLine) {
          nl = paste(nl, splitX[[l]][s], sep = newsplit)
          line = paste(line, splitX[[l]][s], sep = newsplit)
        }
        else {
          nl = paste(nl, splitX[[l]][s], sep = paste0(if (keepSplitAtEOL) 
            newsplit
            else "", "\n"))
          line = splitX[[l]][s]
        }
      }
    newLabels[l] = nl
  }
  substring(newLabels, nchar(newsplit) + 1)
}








textInputRow <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-custom"))
}


textPassword <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "password", value = value, class="input-custom"))
}


myHiddenBoolean <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "checkbox", value = value, class="input-boolean"))
}

myHiddenText <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-text"))
}


####################
# copied from single files
####################


# *****************************
lappend <- function (lst, ...){
  # Append an item to a list; e.g.:
  # vara <- list("axislimit"=T)
  # vara <- lappend(vara, "print"=F)
  lst <- c(lst, list(...))
  return(lst)
}

# *****************************
is.rank <- function(x){
  ranked <- T
  if(any(x < 1)){
    # The data are not an ordered subgroup
    ranked <- F
  }
  if(all(!x==1)){
    # The data are ordered by subgroup, but the base subgroup is missing
    ranked <- F
  }
  return(ranked)
}

# *****************************
notin <- function (vector1, vector2){
  # Return the elements in vector1 that are not in vector2
  el <- which(!(vector1 %in% vector2))  # svae in 'el' the element-locations on v1 not in v2
  if(length(el)==0){  # If there aren't any, return NULL
    return(NULL)
  }
  else{  # Else return the elements
    return (vector1[el])
  }
} 



findCommon <- function(vectora, vectorb){
  # Find the health indicators that are common to every year.
  # vectora: the list of heath indicators
  # vectorb: the years in which the indicators occur
  if(length(vectora)!=length(vectorb)){
    stop("Vectors must be of the same length")
  }
  names(which(rowSums(table(vectora, vectorb))==length(unique(vectorb)))) 
}



convert_names_to_old <- function(current.names, conversion){
  # current.names <- names(maindata)
  indx <- which(current.names%in%conversion$new)
  x <- current.names[indx]
  y <- match(x, conversion$new)
  current.names[indx] <- conversion$old[y]
  current.names
}



# This code comes from the tools package

file_ext <- function (x) 
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}



# Get list of folders with data

get_data_folders <- function(){
  folders <- list.files("data/user-data/")
  #folders <- folders[!folders%in%c("HEAT-data")]
  ss <- unlist(strsplit(folders, "____"))
  folders <- unique(ss[!grepl(".rds|.txt", ss, ignore.case = TRUE)])
  
  if(length(folders) == 0) folders <- c("No existing data available" = "none")
  folders
}


# make encoding unknown so data.table has a non-mixed encoding

make_unknown_encoding <- function(vals){
  vals <- enc2native(vals)
  Encoding(vals) <- "unknown"
  vals
}


plot_decimals_setup <- function(plotData, type){
  
  
  if(nrow(plotData) == 0) return(plotData)
  if(type == "disag"){
    plotData <- mutate(plotData,
                       estimate = round(estimate, 1),
                       lower_95ci = round(lower_95ci, 1),
                       upper_95ci = round(upper_95ci,1),
                       se = round(se, 1),
                       popshare = round(popshare,1),
                       national = round(national,1))
    
  }
  
  if(type == "sum"){
    
    plotData[!plotData$measure%in%c("r", "rci", "mld", "ti"),c("inequal", "se", "estimate",  "se.lowerci", "se.upperci")] <- 
      round(plotData[,c("inequal", "se", "estimate",  "se.lowerci", "se.upperci")],1)
    plotData[plotData$measure == "r",c("inequal", "se", "estimate",  "se.lowerci", "se.upperci")] <- 
      round(plotData[,c("inequal", "se", "estimate",  "se.lowerci", "se.upperci")],2)
    plotData[plotData$measure%in%c("rci", "mld", "ti"),c("inequal", "se", "estimate", "se.lowerci", "se.upperci")] <- 
      round(plotData[,c("inequal", "se", "estimate",  "se.lowerci", "se.upperci")],4)
    
    
  }
  
  plotData
}


# This is an alteration of navbarMenu to allow us to drop a 
# specific tab panel

navbarMenu_drop <- function (title, drop = -1, ..., icon = NULL) 
{
  if(drop<0){tabs <-  list(...)}else {tabs <- list(...)[-drop]}
  structure(list(title = title, tabs = tabs, iconClass = shiny:::iconClass(icon)), 
            class = "shiny.navbarmenu")
}

assignColorsShapes <- function(dimensiondetails ){
  
  # dimensiondetails
  # dimension                       subgroup order maxn dimension_type
  # 1     Economic status           Quintile 1 (poorest)     1    5          other
  # 2     Economic status                     Quintile 2     2    5          other
  # 3     Economic status                     Quintile 3     3    5          other
  # 4     Economic status                     Quintile 4     4    5          other
  # 5     Economic status           Quintile 5 (richest)     5    5          other
  # 6           Education                   No education     1    3          other
  # 7           Education                 Primary school     2    3          other
  # 8           Education             Secondary school +     3    3          other
  # 9  Place of residence                          Rural     0    2          other
  # 10 Place of residence                          Urban     0    2          other
  # 11 Subnational region                    01 barishal     0   50         region
  # 12 Subnational region                  02 chittagong     0   50         region
 
  # this orders by subgroup if non ordered otherwise by
  # order number
  dimensiondetails <- order_dimensions(dimensiondetails)
  
  dimensiondetails <- group_by(dimensiondetails, dimension) %>%  mutate(totn = n())
  
  dimensiondetails$ordered <- !dimensiondetails$order == 0
  
  # create a table showing how many of each type
  info <- distinct(dimensiondetails, maxn, totn, ordered)
  info$greater7 <- info$maxn>7
  info$ncol <- info$maxn
  info$ncol[info$greater7] <- 1
  
  # info
  # maxn ordered          dimension greater7  ncol
  # <int>   <lgl>              <chr>    <lgl> <dbl>
  # 1     5    TRUE    Economic status    FALSE     5
  # 2     3    TRUE          Education    FALSE     3
  # 3     2   FALSE Place of residence    FALSE     2
  # 4    50   FALSE Subnational region     TRUE     1
  
  
  # how many in each category of color. First is ordered multi-level
  # second is categorical multi-level and third is EITHER a dimension
  # with more than 7 categories OR a categorical/ordered that has
  # just one level (National average).
  l_multi_ord <- sum(!info$greater7 & info$ordered & info$maxn!=1)
  l_multi_cat <- sum(!info$greater7 & !info$ordered & info$maxn!=1)
  l_one <- sum(info$greater7 | info$maxn==1)
  
  # for sequential and qualitative these are palettes not colors
  # for single, that is a function that just returns colors
  sequential <- rep(c("blue", "red", "turquoise", "orange", "green", "purple", "yellow", "pink"), 10)
  qualitative <- rep(c("redgreen", "blueorange", "greenpurple", "turquoisepink", "orangeblue", "bluered", "purplegreen"), 10)
  single <- rep(pal_single(), 10) # just gives a list of colors
  
  
  # now pull out the palettes (or individual colors) you'll need
  multi_ord <- sequential[1:l_multi_ord]
  multi_cat <- qualitative[1:l_multi_cat]
  multi_single <- single[1:l_one]
  
  # assign colors or palettes to the dimensions
  info$palette <- NA
  info$palette[!info$greater7 & info$ordered & info$maxn!=1] <- multi_ord
  info$palette[!info$greater7 & !info$ordered & info$maxn!=1] <- multi_cat
  info$palette[info$greater7 | info$maxn==1] <- multi_single
  
  
  # now the convoluted process of assigning colors. First
  # we create a list with a level per dimension and the
  # the colors we need
  tmpcols <- suppressWarnings(mapply(function(w, x, y, z){
    
    if(y == 1){
      cols <- rep(z, w)
    }else{
      if(x){# if ordered
        cols <- pal_ordered(y, z)
      }else{
        cols <- pal_nonordered(y, z)
      }
      
    }
    return(cols)
    
  }, info$totn, info$ordered, info$ncol, info$palette))
  
  # here is tmpcolos
  # [[1]]
  # [1] "#B2FF86" "#7CDD49" "#45A500" "#007000" "#004000"
  # 
  # [[2]]
  # [1] "#BBFFFF" "#00A698" "#004D3A"
  # 
  # [[3]]
  # [1] "#007300"
  # 
  # [[4]]
  # [1] "#00CD8D" "#FF7A9E"
  # 
  # [[5]]
  # [1] "#00AECF" "#CA8200"
  # 
  # [[6]]
  # [1] "#00B39C"
  
  # we're almost there. For the non-single colors we have the
  # right number of colors
  
  # cols <- unlist(sapply(1:nrow(info), function(x){
  #   
  #   if(info$ncol[x] == 1){return(rep(tmpcols[[x]][1], info$totn[x]))}else{
  #     return(tmpcols[[x]])
  #   }
  # }))

  dimensiondetails$colors <- c(unlist(tmpcols))
  dimensiondetails$shapes <- 21
  dimensiondetails
  
  
}



 
updateFocusYear <- function(yr = yr){
  
  if(all(yr%in%.rdata[['focus_year']])){
    .rdata[['focus_year']] <<- unique(c(yr, .rdata[['focus_year']]))
  }
}

# ------ Alternate select
# We had a situation where if the use had selected data sources
# in the order MICS, DHS rather than DHS, MICS the select
# was getting re-ordered. We don't want this

selectInputWHO <- function (inputId, label, choices, selected = NULL, multiple = FALSE, 
                            selectize = TRUE, width = NULL, size = NULL, keep.order = FALSE) 
{
  inputId <- "a"
  label = ""
  choices <- c("a", "b" , "c")
  selected <- c("b", "a")
  
  selected <- restoreInput(id = inputId, default = selected)
  choices <- shiny:::choicesWithNames(choices)
  if (is.null(selected)) {
    if (!multiple) 
      selected <- firstChoice(choices)
  }
  else selected <- validateSelected(selected, choices, inputId)
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  selectTag <- tags$select(id = inputId, class = if (!selectize) 
    "form-control", size = size, selectOptionsWHO(choices, selected, keep.order = keep.order))
  if (multiple) 
    selectTag$attribs$multiple <- "multiple"
  res <- div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), controlLabel(inputId, 
                                                                 label), div(selectTag))
  if (!selectize) 
    return(res)
  selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% 
                                                              choices))
}


selectOptionsWHO <- function (choices, selected = NULL, keep.order = TRUE) 
{
  html <- mapply(choices, names(choices), FUN = function(choice, 
                                                         label) {
    
    if (is.list(choice)) {
      sprintf("<optgroup label=\"%s\">\n%s\n</optgroup>", 
              htmlEscape(label, TRUE), shiny:::selectOptions(choice, 
                                                             selected))
    }
    else {
      sprintf("<option value=\"%s\"%s>%s</option>", htmlEscape(choice, 
                                                               TRUE), if (choice %in% selected) 
                                                                 " selected"
              else "", htmlEscape(label))
    }
  })
  
  if(keep.order){
    a <- match(selected, names(html))
    html <- c(html[a], html[-a])
  }
  
  HTML(paste(html, collapse = "\n"))
}

debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}

busyIndicator <- function(text = "",img = "spinner.gif", wait=1000) {
  tagList(
    div(class="busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.busy-indicator').show()
      }
      }, %d)
      } else {
      $('div.busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )	
}

barWidth <- function(cnts, detailedBar = FALSE){
  # nrows <- nrow(plotData)
  
  if(detailedBar){
    if(cnts == 1) return(50)
    if(cnts == 2) return(50)
    if(cnts == 3) return(50)
    return(200/cnts)
  }else{
    if(cnts == 1) return(50)
    if(cnts == 2) return(50)
    if(cnts == 3) return(50)
    return(NULL) 
  }
  
  #20 + 100/nrow(plotData)
  
}


conf.int.norm <- function(val, se){
  l <- val - se * qnorm(0.975)
  u <- val + se * qnorm(0.975)
  
  return(list(l = l, u = u))
}



order_dimensions <- function(dat){

  dimension_ordered <- dplyr::filter(dat, order != 0) %>% 
    dplyr::arrange(dimension, order)
  
  dimension_nonordered <- dplyr::filter(dat, order == 0) %>% 
    arrange(dimension, subgroup)
  
  dat <- rbind(dimension_ordered, dimension_nonordered)
  
  dat$subgroup <- factor(dat$subgroup, levels = unique(dat$subgroup))
  dat
}







pal_ordered <- function (n, col, l = c(20, 100), power = 1,
                         fixup = TRUE, gamma = NULL, alpha = 1, ...)
{
  
  vals <- switch(col,
                 "green" = list(h = c(120, 0), c. = c(80, 100)),
                 "turquoise" = list(h = 180, c. = c(80, 40)),
                 "blue" = list(h = 260, c. = c(80, 100)),
                 "purple" = list(h = 280, c. = c(80, 100)),
                 "pink" = list(h = 320, c. = c(80, 100)),
                 "red" = list(h = 10, c. = c(80, 100)),
                 "orange" = list(h = 30, c. = c(80, 100)),
                 "yellow" = list(h = 60, c. = c(80, 100)),
                 "lightgreen" = list(h = 100, c. = c(80, 100)),
                 "lightblue" = list(h = 210, c. = c(80, 40)),
                 NULL)
  
  if(is.null(vals)) stop("Wrong choice of color palette")
  
  h <- vals$h
  c. <- vals$c.
  
  
  if (!is.null(gamma))
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L)
    return(character(0L))
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L],
                       C = c[2L] - diff(c) * rval^power[1L], H = h[1L]), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}




#plot(1:10, 1:10, pch = 16, cex = 1.3, col = pal_nonordered(10, "blueorange"))

pal_nonordered <- function (n, col, c = 100, fixup = TRUE,
                            gamma = NULL, alpha = 1, ...)
{
  
  
  vals <- switch(col,
                 "redgreen" = list(l = 70, start = -360, end = -200),
                 "orangeblue" = list(l = 60, start = -310, end = -150),
                 "greenpurple" = list(l = 50, start = -260, end = -100),
                 "turquoisepink" = list(l = 70, start = -210, end = -50),
                 "bluered" = list(l = 60, start = -160, end = 0),
                 "blueorange" = list(l = 50, start = -110, end = 50),
                 "orangeblue" = list(l = 70, start = -60, end = -100),
                 NULL)
  
  if(is.null(vals)) stop("Wrong choice of color palette")
  
  l <- vals$l
  start <- vals$start
  end <- vals$end
  
  if (!is.null(gamma))
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L)
    return(character(0L))
  rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)),
              fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}



# taken from package colorspace
hex <- function (from, gamma = NULL, fixup = FALSE) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  coords <- as(from, "sRGB")@coords
  rval <- .Call("sRGB_to_RColor", coords, fixup, PACKAGE = "colorspace")
  if (!is.null(dnam <- attr(coords, "dimnames"))) 
    names(rval) <- dnam[[1L]]
  rval <- rev(rval)
  return(rval)
}

# taken from package colorspace
polarLUV <- function (L, C, H, names) 
{
  if (missing(L)) 
    return(new("polarLUV"))
  if (missing(names)) 
    names = dimnames(L)[[1]]
  coords = cbind(L, if (missing(C)) 
    NULL
    else C, if (missing(H)) 
      NULL
    else H)
  dimnames(coords) = list(names, c("L", "C", "H"))
  new("polarLUV", coords = coords)
}


pal_single <- function(){
  c("green" = "#007300",
    "turquoise" = "#00B39C",
    "blue" = "#003AC3",
    "purple" = "#7D00EC",
    "pink" = "#D65EE9",
    "red" = "#CE4951",
    "orange" = "#DE7429",
    "yellow" = "#FFDA3D",
    "lightgreen" = "#B0D500",
    "lightblue" = "#00AECF")
}





