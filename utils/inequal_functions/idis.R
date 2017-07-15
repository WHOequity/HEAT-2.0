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

######### Index of Disparity (IDis)
# The Index of Disparity summarizes the difference between several group rates
# and a reference rate and expresses the average differences as a proportion of 
# the reference rate (Keppel & Pearcy, 2002: http://www.ncbi.nlm.nih.gov/pubmed/12432138).
# 
# 
#
#
#########dat


idis <- function(dat, bs=TRUE, bsiter = 200){
  # relative, complex, not ordered, weighted, greater than two
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
                         #missing_natl_avg,
                         estimates_all_zero,
                         #natl_all_zero,
                         less_than2_subgroups)
  
 

  se.formula <- NA
  se.boot <- NA
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)

  if(any(badData) || is.na(est_natl)) return(na_return)
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  est_natl1 <- est_natl/scaleval

    #inequal.idis <- (sum(abs(est - est_natl)) /(length(est)))/est_natl*100
    inequal.idis <- (sum(abs(est1 - est_natl1)) /(length(est1)))/est_natl1 * 100
    
    if(SEuseful){
    # Set the looping number
    n <- bsiter# this was 10k in the script I was sent
    
    inequal.boot.vals <- sapply(1:n, function(x){
      
      estrand <- rnorm(length(est1), est1, se1)
      # If scaleval =100, only the simulations where all estimates (components of est) are between 0 and 1 should be used.
      # If scaleval=1000 or otherwise, only the simulations where all estimates (components of est) are positive should be used.
      
      if ((scaleval == 100 & all(estrand >= 0) & all(estrand <= 1)) |
          (scaleval != 100 & all(estrand >= 0))) {
        # The weighted mean calculated for each simulation was used instead of the national average.
        (sum(abs(estrand - sum(popsh * estrand))) /(length(estrand)))/sum(popsh * estrand)*100
      }
    })
    
    inequal.boot.vals <- unlist(inequal.boot.vals)
    
    # Method : instead of providing the SE, the 95% CIs were constructed by the quantiles
    # of the valid simulated values (the 2.5 and 97.5 percentiles)
    boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)
}
    
  return(list(inequal.idis=inequal.idis, se.idis.boot=se.boot,  se.idis.formula=se.formula, se.lowerci.idis = boot.lcl2, se.upperci.idis = boot.ucl2))
  
}
