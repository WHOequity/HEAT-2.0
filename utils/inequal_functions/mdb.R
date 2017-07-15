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

######### Mean Difference from the Best performing Subgroup (mdb)


mdb <- function(dat, bs=TRUE, bsiter = 200){
  # absolute, complex, non ordered, weighted, greater than two
  # region
  # TEST WITH dat <- disagdata.split[[4]][[1]]
  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         is_ordered,
                         missing_estimates,
                         estimates_all_zero,
                         less_than2_subgroups)
  
  if(any(badData)) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  rankable <- is_ordered(dat)
  positive <- is_positive(dat)
  
  
  if(!hasrefgroup) ref_est <- ifelse(positive, max(est), min(est))
  if(hasrefgroup) ref_est <- dat$estimate[dat$reference_subgroup == 1]
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  ref_est1 <- ref_est / scaleval
  
  inequal.mdb <- sum(popsh * abs(ref_est1-est1))
  
  
  se.formula <- NA
  se.boot <- NA
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
 if(SEuseful){
    if(bs){
      inequal.boot.vals <- sapply(1:bsiter, function(x){
        estrand <- rnorm(length(est1), est1, se1)
        # If scaleval =100, only the simulations where all estimates (components of est) are between 0 and 1 should be used.
        # If scaleval=1000 or otherwise, only the simulations where all estimates (components of est) are positive should be used.
        
        if ((scaleval == 100 & all(estrand >= 0) & all(estrand <= 1)) | (scaleval != 100 & all(estrand >= 0))) {
          # The weighted mean calculated for each simulation was used instead of the national average.
          scaleval * sum(popsh * abs(ref_est1 - estrand))
          
        }
      })
    }
  
  inequal.boot.vals <- unlist(inequal.boot.vals)
  inequal.mdb <- inequal.mdb * scaleval
  boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
  boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)
 }
  # Return the results as a list
  return(list(inequal.mdb=inequal.mdb, se.mdb.boot=se.boot,  se.mdb.formula=se.formula, se.lowerci.mdb = boot.lcl2, se.upperci.mdb = boot.ucl2))  # return a list of the inequality measure and the standard error 
  
}
