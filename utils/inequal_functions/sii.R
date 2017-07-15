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

######### Slope Index of Inequality (SII)


sii <- function(dat, bs=FALSE, bsiter = 200){
  
  
  # relative, complex, ordered, weighted, greater than two
  # wealth, educ
  # TEST WITH: dat <- disagdata.split[[5]][[1]]
  
  dat$subgroup.new <- (dat$favourable_indicator-0.5) * dat$subgroup_order
  dat <- dat[order(dat$country, dat$year, dat$indicator_abbr, dat$dimension, dat$subgroup.new),]
  
  #dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         not_ordered,
                         missing_estimates,
                         missing_population,
                         estimates_all_zero,
                         se_all_missing,
                         has_zero_se,
                         less_than2_subgroups)
  
  if(any(badData) || !SEuseful) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  # scaleval <- ifelse(scaleval == 10, 1, scaleval)
  if (scaleval == 1) {
    ################ code for scaleval=1 (use the formula from HD*Calc) #################
    # Calculating p_j, X_j

    pX <- group_by(dat, country, year, indicator_abbr, dimension) %>% 
      mutate(
                sum.pop = sum(population),
                p_j = pop/sum.pop,
                cumsum.pop = cumsum(pop),
                retain.pop = cumsum.pop - pop,
                X_j = (retain.pop + pop/2) / sum.pop
    )
    
    # Calculating sii, standard error, lower ci and upper ci of sii:
    rst <- group_by(pX, country, year, source, indicator_abbr, indicator_name, dimension, favourable_indicator, indicator_scale) %>% 
      summarize(
                 sii = (sum(p_j*X_j*estimate) - sum(p_j*X_j)*sum(p_j*estimate)) /
                   (sum(p_j*X_j^2) - (sum(p_j*X_j))^2),
                 se.sii = ((sum(p_j^2*X_j^2*se^2) + sum(p_j*X_j)^2*sum(p_j^2*se^2)
                            - 2*sum(p_j*X_j)*sum(p_j^2*X_j*se^2)) / (sum(p_j*X_j^2) - (sum(p_j*X_j))^2)^2)^0.5, 
                 lcl.sii = sii - 1.96 * se.sii,
                 ucl.sii = sii + 1.96 * se.sii
    )

    
    sii <- rst$sii
    se.sii <- rst$se.sii
    
  } else if (scaleval == 100) {
    ################ code for proportions (scaleval=100) #################
    # use the scaleval to rescale (for each subpopulation) the est and se to correspond to percentages

    est1 <- est/scaleval
    se1 <- se/scaleval
    
    # The proportions must be have positive numbers
    if (any(est1 < 0) | any(est1 > 1)) {
      stop('All proportions should be between 0 and 1.')
    }
    
    # calculate the effective sample size (for each subpopulation)
    ess <- ( est1 * (1 - est1) ) / (se1 ^ 2)
    
    # calculate the subpopulation counts (to be used for the logistic regression model)
    est <- round(est1 * ess)
    
    # calculate the subpopulation sizes (to be used for the logistic regression model)
    ess <- round(ess)
    
    # The ranks will still be calculated using old pop
    rank <- midPointProp(pop)
    
    # Logistic regression
    mlogit <- summary(glm(cbind(est, ess-est)~rank,family=binomial(link="logit"))) 
    coefs <- mlogit$coefficients[,1]
    alpha <- mlogit$coefficients[1,1]
    beta <- mlogit$coefficients[2,1]
    vcov1 <- vcov(mlogit)
    p1 <- exp(alpha + beta) / (1 + exp(alpha + beta))
    p0 <- exp(alpha) / (1 + exp(alpha))
    
    # SII
    sii <- scaleval * (p1 - p0) 
    se.sii <- scaleval * deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                                     coefs, vcov1)

    
  } else if (scaleval != 1 & scaleval != 100 ) {
    ################ code for  rates ( scaleval=1000 or others) #################
    # use the scaleval to rescale (for each subpopulation) the est and se to correspond to percentages
    
    est1 <- est/scaleval
    se1 <- se/scaleval
    
    # The rates must be have positive numbers
    if (any(est1 < 0)) {
      stop('All rates should be positive numbers.')
    }
    
    # calculate the effective sample size (for each subpopulation)
    ess <- est1 / (se1 ^ 2)
    
    # calculate the subpopulation counts (to be used for the Poisson regression model)
    est <- round(est1 * ess)
    
    # calculate the subpopulation sizes (to be used for the Poisson regression model)
    ess <- round(ess)
    
    # The ranks will still be calculated using old pop
    rank <- midPointProp(pop)
    
    # Multiplicative Poisson regression
    mpois <- summary(glm(est~offset(log(ess))+rank,family=poisson(link="log")))
    coefs <- mpois$coefficients[,1]
    vcov1 <- vcov(mpois)
    alpha <- mpois$coefficients[1,1]
    beta <- mpois$coefficients[2,1]
    p1 <- exp(alpha + beta)
    p0 <- exp(alpha)
    
    # SII
    sii <- scaleval * (p1 - p0) 
    se.sii <- scaleval * deltamethod(~ (exp(x1+x2) - exp(x1)),coefs, vcov1)
    
  }
  
  inequal.sii <- sii
  se.formula <- se.sii
  se.boot <- NA

  ci <- list(l = NA, u = NA)
  if(SEuseful){
    ci <- conf.int.norm(inequal.sii, se.formula)
  }
  
  # Return the results as a list
  return(list(inequal.sii=inequal.sii, se.sii.boot=se.boot,  se.sii.formula=se.formula, se.lowerci.sii = ci$l, se.upperci.sii = ci$u))  # return a list of the inequality measure and the standard error 
}
