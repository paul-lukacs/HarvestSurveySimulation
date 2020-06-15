# Functions needed for surveys and analysis: ===================================
# simple() =====================================================================
# 
# Creates a tibble containing a simple random sample of hunters
#
# simple(n, 
#        split = 0.7, 
#        pSuccess1, 
#        pSuccess2 = pSucces, 
#        pSample,
#        pResp, 
#        scale.pResp = 1, 
#        fus = FALSE, 
#        fus.scale = NULL)
#
# n             Desired population size.
# split         Proportion of the population that is placed into group 1
# pSuccess1     Probability of hunter in group 1 to harvest
# pSuccess2     Probability of hunter in group 2 to harvest
# pSample       Probability a hunter is chosen for survey
# pResp         Response probability/probabilities for UNSUCCESSFUL hunters.
# scale.pResp   Scales pResp to create probabilities for SUCCESSFUL hunters.
#                 should be > 1
# fus           Logical. Change to TRUE if follow up surveys to be done
# fus.scale     Scales probabilities of response to initial surveys, creating
#                 new probabilities of response for follow up surveys.
#                 must be < 1. 

simple <- function(n, split = 0.7, pSuccess1, pSuccess2 = pSuccess1, pSample,
                   pResp, scale.pResp = 1, fus = FALSE, fus.scale = NULL){
  
  # Create initial population data:
  init_pop <- tibble(
    pop_size     = n,
    group        = rbinom(n, 1, split),          # assigning group
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, pSuccess1),      # Group 1 harvest sim
      group == 0 ~ rbinom(n, 1, pSuccess2)),     # Group 0 harvest sim
    true_harvest = sum(harvest),                 # total pop. harvest
    sample       = rbinom(n, 1, pSample)         # Sample or not
  )
  
  # Quick warning if scale.pResp is less than 1:
  if (scale.pResp < 1){
    warning(
      paste0("scale.pResp < 1; successful hunters will be less likely to",
             " respond to survey than unsuccessful hunters"),
      call. = FALSE
    )
  }
  
  # Now take initial population data, and make copies of it:
  full_sim <- map_dfr(seq_along(pResp), ~ init_pop) %>%
    # Now add columns for response:
    mutate(
      uns_resp_rate = rep(pResp, each = n),
      suc_resp_rate = rep(pResp, each = n)*scale.pResp,
      # if suc_resp_rate is > 1, make it = 1:
      suc_resp_rate = 
        case_when(
          suc_resp_rate > 1 ~ 1,
          TRUE              ~ suc_resp_rate
        ),
      # and then simulate response:
      init_resp = 
        case_when(
          sample  == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(n*length(pResp), 1, suc_resp_rate),
          harvest == 0 ~ rbinom(n*length(pResp), 1, uns_resp_rate),
          TRUE         ~ NA_integer_
        )
    )
  
  # Follow up survey:
  if (fus){
    
    # if follow up surveys are to be done, the argument must be provided:
    if (is.null(fus.scale)){
      stop("Specify fus.scale argument", call. = FALSE)
    }
    
    if (fus.scale > 1){
      stop("fus.scale > 1; more likely to respond to follow up than initial",
           call. = FALSE)
    }
    
    # Create response probabilities column for follow up survey and simulate:
    full_sim <- full_sim %>% 
      mutate(
        # Determine new response rates to follow ups:
        fus_uns_resp_rate = uns_resp_rate*fus.scale,
        fus_suc_resp_rate = suc_resp_rate*fus.scale,
        # Simulate response to follow ups:
        fus_resp = 
          case_when(
            sample    == 0  ~ NA_integer_,
            init_resp == 1  ~ NA_integer_,
            harvest   == 1  ~ rbinom(n*length(pResp), 1, fus_suc_resp_rate),
            harvest   == 0  ~ rbinom(n*length(pResp), 1, fus_uns_resp_rate),
            TRUE            ~ NA_integer_
          )
      )
  }
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# mand() =======================================================================
# 
# Creates a tibble containing a simulation where successful hunters are mandated
# to report. 
#
# mand (n, 
#       split = 0.7, 
#       pSuccess1, 
#       pSuccess2 = pSuccess1, 
#       pResp)
#
# n             Desired population size.
# split         Proportion of the population that is placed into group 1
# pSuccess1     Probability of hunter in group 1 to harvest
# pSuccess2     Probability of hunter in group 2 to harvest
# pResp         Response probability/probabilities for **SUCCESSFUL** hunters.
#                 as they are the only ones responding.

mand <- function(n, split = 0.7, pSuccess1, pSuccess2 = pSuccess1, pResp){
  
  # Create initial population data:
  init_pop <- tibble(
    pop_size     = n,
    group        = rbinom(n, 1, split),          # assigning group
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, pSuccess1),      # Group 1 harvest sim
      group == 0 ~ rbinom(n, 1, pSuccess2)),     # Group 0 harvest sim
    true_harvest = sum(harvest),                 # total pop. harvest
  )
  
  # Now take initial population data, and make copies of it:
  full_sim <- map_dfr(seq_along(pResp), ~ init_pop) %>%
    # Now add columns for response:
    mutate(
      resp_rate = rep(pResp, each = n),
      # and then simulate response:
      init_resp = 
        case_when(
          harvest == 1 ~ rbinom(n*length(pResp), 1, resp_rate),
          TRUE         ~ 0L
        )
    )
  
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# voluntary() ==================================================================
#
# Creates a tibble of a simulation where hunters self-report.
#
# voluntary <- (n,
#               split = 0.7,
#               pSuccess1,
#               pSuccess2 = pSuccess1,
#               pResp,
#               scale.pResp = 1,
#               fus = FALSE,
#               fus.pSample = NULL,
#               fus.scale = NULL)
#
# n             Desired population size.
# split         Proportion of the population that is placed into group 1
# pSuccess1     Probability of hunter in group 1 to harvest
# pSuccess2     Probability of hunter in group 2 to harvest
# pResp         Response probability/probabilities for **UNSUCCESSFUL** hunters.
# scale.pResp   Scales pResp to create probabilities for SUCCESSFUL hunters.
#                 should be > 1
# fus           Logical. Change to TRUE if follow up surveys to be done
# fus.pSample   Probability a non-respondent is chosen for follow up survey.
# fus.scale     Scales probabilities of response to initial surveys, creating
#                 new probabilities of response for follow up surveys.

voluntary <- function(n, split = 0.7, pSuccess1, pSuccess2 = pSuccess1,
                      pResp, scale.pResp = 1, fus = FALSE, 
                      fus.pSample = NULL, fus.scale = NULL){
  
  # Function changeto1 
  # used when a probability is scaled to >1, brings prob back down to 1. 
  changeto1 <- function(x){
    case_when(
      x > 1 ~ 1,
      TRUE  ~ x
    )
  }
  
  # Create initial population data:
  init_pop <- tibble(
    pop_size     = n,
    group        = rbinom(n, 1, split),          # assigning group
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, pSuccess1),      # Group 1 harvest sim
      group == 0 ~ rbinom(n, 1, pSuccess2)),     # Group 0 harvest sim
    true_harvest = sum(harvest),                 # total pop. harvest
  )
  
  # Quick warning if scale.pResp is less than 1:
  if (scale.pResp < 1){
    warning(
      paste0("scale.pResp < 1; successful hunters will be less likely to",
             " respond to survey than unsuccessful hunters"),
      call. = FALSE
    )
  }
  
  # Now take initial population data, and make copies of it:
  full_sim <- map_dfr(seq_along(pResp), ~ init_pop) %>%
    # Now add columns for response:
    mutate(
      uns_resp_rate = rep(pResp, each = n),
      suc_resp_rate = rep(pResp, each = n)*scale.pResp,
      # if suc_resp_rate is > 1, make it = 1:
      suc_resp_rate = changeto1(suc_resp_rate),
      # and then simulate response:
      init_resp = 
        case_when(
          harvest == 1 ~ rbinom(n*length(pResp), 1, suc_resp_rate),
          harvest == 0 ~ rbinom(n*length(pResp), 1, uns_resp_rate),
          TRUE         ~ NA_integer_
        )
    )
  
  # If the TRUE statement above added NA's, something went wrong:
  if (some(full_sim$init_resp, is.na)){
    stop("NA in init_resp when there shouldn't be.", call. = FALSE)
  }
  
  # Follow up survey:
  if (fus){
    
    # if follow up surveys were to be done, the arguments must be provided:
    if (is.null(fus.pSample) | is.null(fus.scale)){
      stop("Specify follow up survey arguments", call. = FALSE)
    }
    
    # Create response probabilities column for follow up survey and simulate:
    full_sim <- full_sim %>% 
      mutate(
        # Determine new response rates to follow ups:
        fus_uns_resp_rate = uns_resp_rate*fus.scale,
        fus_suc_resp_rate = suc_resp_rate*fus.scale,
        # if follow up response rates are > 1, make them = 1:
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        # Sample non-respondents:
        fus_sample = case_when(
          init_resp == 0 ~ rbinom(n*length(pResp), 1, fus.pSample),
          init_resp == 1 ~ NA_integer_,
          TRUE           ~ NA_integer_
        ),
        # Simulate response to follow ups:
        fus_resp = 
          case_when(
            init_resp  == 1    ~ NA_integer_,
            fus_sample == 0    ~ NA_integer_ ,
            harvest    == 1    ~ rbinom(n*length(pResp), 1, fus_suc_resp_rate),
            harvest    == 0    ~ rbinom(n*length(pResp), 1, fus_uns_resp_rate),
            TRUE               ~ NA_integer_
          )
      )
    
    if (fus.scale < 1L){
      warning(paste0("fus.scale < 1; less likely to respond to follow up",
                     " than to voluntarily report"),
              call. = FALSE)
    }
  }
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# Estimator functions ==========================================================

# All estimator functions report an estimate of harvest, standard error,
# and relative bias, calculated as (est harvest / true harvest) - 1, among
# other important metadata. Returns a tibble.

# All estimator functions, except s2_estimator(), use svytotal() to estimate
# total harvest, and a fpc to scale standard error. 

# All estimator functions are called by scenario function (described below),
# so these functions never need to be explicitly typed out. 

# s1_estimator() ===============================================================
#
# Calculates harvest estimates for scenario 1.
# It creates two survey designs using survey::svydesign(). One for inital 
# respondents, and another for follow up respondents. The function then returns 
# svytotal()  outputs for both of these designs, and reports the true harvest 
# for pop. It also creates a combined estimate between the two. 
# Can be used outside of map(), but designed to be the .f argument 
# in map()
#
# s1_estimator(x, level)
#
# x     = the data, an output from simple() function, where a follow up is done.
# level = the desired level of response to use in the survey design. Described
#           as desired value in x$uns_resp_rate

s1_estimator <- function(x, level){
  
  # Filter down to the population and response rate specified:
  pop <- x %>% 
    filter(near(uns_resp_rate, level))
  # Filter down to intial respondents only.
  init_resp_only <- pop %>% 
    filter(init_resp == 1)
  
  # create design:
  init_design <- svydesign(ids   = ~1,
                           probs = nrow(init_resp_only)/nrow(pop), 
                           data  = init_resp_only, 
                           fpc   = ~pop_size)
  
  # create strata table for post-stratification:
  strata <- data.frame(
    group = c(1, 0),
    Freq  = c(sum(pop$group), pop$pop_size[[1]]-sum(pop$group))
  )
  
  # post-stratify:
  init_design <- postStratify(init_design, ~group, strata)
  
  #estimate total harvest:
  init_est <- svytotal(~harvest, init_design)
  
  # Now do the same for follow up respondents, but if reponse level was
  # already 1, there is nobody to follow up with, so don't bother:
  if(level < 1){
    fus_resp_only <- pop %>% 
      filter(fus_resp == 1)
    
    # create design for follow up respondents:
    fus_design <- svydesign(ids   = ~1,
                            probs = nrow(fus_resp_only)/nrow(pop), 
                            data  = fus_resp_only, 
                            fpc   = ~pop_size)
    
    # post-stratify:
    fus_design <- postStratify(fus_design, ~group, strata)
    
    #estimate total harvest:
    fus_est <- svytotal(~harvest, fus_design)
    
    # now combine the two estimates for a total est:
    # Step 1, define scaling proportions:
    init_prop <- sum(pop$init_resp, na.rm = TRUE)/sum(pop$sample)
    fus_prop  <- 1-init_prop  # assume fus respondents make the rest of the pop.
    
    # Step 2, apply proportions and combine:
    combined_est <- (init_est*init_prop)+(fus_est*fus_prop)
    
    # Create output:
    out <- tibble(
      scenario        = 1L,
      resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
      response_rate   = level,
      initial_est     = as.vector(init_est),
      initial_SE      = as.vector(SE(init_est)),
      follow_up_est   = as.vector(fus_est),
      follow_up_SE    = as.vector(SE(fus_est)),
      combined_est    = as.vector(combined_est),
      true_harvest    = x$true_harvest[[1]],
      relative_bias   = (as.vector(combined_est) / x$true_harvest[[1]]) - 1
    )
  }
  
  else{
    
    # Create output:
    out <- tibble(
      scenario        = 1L,
      resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
      response_rate   = level,
      initial_est     = as.vector(init_est),
      initial_SE      = as.vector(SE(init_est)),
      true_harvest    = x$true_harvest[[1]],
      relative_bias   = (as.vector(init_est) / x$true_harvest[[1]]) - 1
    )
  }
  return(out)
}


# s2_estimator() ==============================================================
#
# Simple. no svydesign(). Assumes 100% reporting by successful hunters. 
# 
# x = an output from mand()


s2_estimator <- function(x){
  out <- x %>% 
    group_by(resp_rate) %>% 
    
    summarise(
      scenario      = 2L,
      resp_bias     = NA,
      estimate      = sum(init_resp, na.rm = TRUE),
      estimate_SE   = 0,
      true_harvest  = mean(true_harvest),
      relative_bias = (estimate / true_harvest) - 1,
    )
  
  #re-arrange so data is similar looking to other functions:
  out <- out %>%
    select(scenario, resp_bias, resp_rate, everything()) %>% 
    rename(response_rate = resp_rate)
  
  return(out)
}

# s3_estimator() ===============================================================
#
# Calculates harvest estimates for scenario 3. 
# 

s3_estimator <- function(x, level){
  
  # Filter down to the population and response rate specified:
  pop <- x %>% 
    filter(near(uns_resp_rate, level))
  # Filter down to intial respondents only.
  init_resp_only <- pop %>% 
    filter(init_resp == 1)
  
  # create design:
  init_design <- svydesign(ids   = ~1,
                           probs = nrow(init_resp_only)/nrow(pop), 
                           data  = init_resp_only)
  
  #estimate total harvest:
  init_est <- svytotal(~harvest, init_design)
  
  # Create output:
  out <- tibble(
    scenario        = 3L,
    resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
    response_rate   = level,
    initial_est     = as.vector(init_est),
    initial_SE      = as.vector(SE(init_est)),
    true_harvest    = x$true_harvest[[1]],
    relative_bias   = (as.vector(init_est) /x$true_harvest[[1]]) - 1
  )
  
  return(out)
}


# s4_estimator() ===============================================================
#
# Creates harvest estimates for scenario 4
# reports svytotal() for initial respondents harvest among other data
#

s4_estimator <- function(x, level){
  
  # Filter down to the population and response rate specified:
  pop <- x %>% 
    filter(near(uns_resp_rate, level))
  # Filter down to intial respondents only.
  init_resp_only <- pop %>% 
    filter(init_resp == 1)
  
  # create design:
  init_design <- svydesign(ids   = ~1,
                           probs = nrow(init_resp_only)/nrow(pop), 
                           data  = init_resp_only, 
                           fpc   = ~pop_size)
  
  # create strata table for post-stratification:
  strata <- data.frame(
    group = c(1, 0),
    Freq  = c(sum(pop$group), pop$pop_size[[1]]-sum(pop$group))
  )
  
  # post-stratify:
  init_design <- postStratify(init_design, ~group, strata)
  
  # estimate total harvest:
  init_est <- svytotal(~harvest, init_design)
  
  # now create output:
  out <- tibble(
    scenario        = 4L,
    resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
    response_rate   = level,
    initial_est     = as.vector(init_est),
    initial_SE      = as.vector(SE(init_est)),
    true_harvest    = x$true_harvest[[1]],
    relative_bias   = (as.vector(init_est) / x$true_harvest[[1]]) - 1
  )
  
  return(out)
}
# s5_estimator() ===============================================================

# This is the same exact function as s1_estimator() except for a single line. 
# When creating proportions to merge two estimates, there is no "sample" column,
# as this is applied to a voluntary response scenario. So in s1_estimator()
# proportion of initial survey est is assumed to be # of responses / # sampled.
# but now in this function, the respondents are the actual proportion of pop,
# so it is # of respondents / pop. size

s5_estimator <- function(x, level){
  
  # Filter down to the population and response rate specified:
  pop <- x %>% 
    filter(near(uns_resp_rate, level))
  # Filter down to intial respondents only.
  init_resp_only <- pop %>% 
    filter(init_resp == 1)
  
  # create design:
  init_design <- svydesign(ids   = ~1,
                           probs = nrow(init_resp_only)/nrow(pop), 
                           data  = init_resp_only, 
                           fpc   = ~pop_size)
  
  # create strata table for post-stratification:
  strata <- data.frame(
    group = c(1, 0),
    Freq  = c(sum(pop$group), pop$pop_size[[1]]-sum(pop$group))
  )
  
  # post-stratify:
  init_design <- postStratify(init_design, ~group, strata)
  
  #estimate total harvest:
  init_est <- svytotal(~harvest, init_design)
  
  # Now do the same for follow up respondents, but if reponse level was
  # already 1, there is nobody to follow up with, so don't bother:
  if(level < 1){
    fus_resp_only <- pop %>% 
      filter(fus_resp == 1)
    
    # create design for follow up respondents:
    fus_design <- svydesign(ids   = ~1,
                            probs = nrow(fus_resp_only)/nrow(pop), 
                            data  = fus_resp_only, 
                            fpc   = ~pop_size)
    
    # post-stratify:
    fus_design <- postStratify(fus_design, ~group, strata)
    
    #estimate total harvest:
    fus_est <- svytotal(~harvest, fus_design)
    
    # now combine the two estimates for a total est:
    # Step 1, define scaling proportions:
    init_prop <- mean(pop$init_resp, na.rm = TRUE) # The prop. of respondents to 
                # self-report IS the proportion of the population who reported. 
    fus_prop  <- 1-init_prop  # assume fus respondents make the rest of the pop.
    
    # Step 2, apply proportions and combine:
    combined_est <- (init_est*init_prop)+(fus_est*fus_prop)
    
    # Create output:
    out <- tibble(
      scenario        = 5L,
      resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
      response_rate   = level,
      initial_est     = as.vector(init_est),
      initial_SE         = as.vector(SE(init_est)),
      follow_up_est   = as.vector(fus_est),
      follow_up_SE    = as.vector(SE(fus_est)),
      combined_est    = as.vector(combined_est),
      true_harvest    = x$true_harvest[[1]],
      relative_bias   = (as.vector(combined_est) / x$true_harvest[[1]]) - 1
    )
  }
  
  else{
    
    # Create output:
    out <- tibble(
      scenario        = 5L,
      resp_bias       = x$suc_resp_rate[[1]]/x$uns_resp_rate[[1]],
      response_rate   = level,
      initial_est     = as.vector(init_est),
      initial_SE      = as.vector(SE(init_est)),
      true_harvest    = x$true_harvest[[1]],
      relative_bias   = (as.vector(init_est) / x$true_harvest[[1]]) - 1
    )
  }
  return(out)
}

# Sim and analysis functions ===================================================

# s1, s3, s4, s5 all take one argument, the response bias desired. intended to
# be passed by map() function. 

# e.g.: map_dfr(c(1, 1.2, 1.4), s1) will evaluate s1 at each response bias given
# simulating population using simple() and obtaining estimates using 
# s1_estimator.

# Should be used as map_dfr(c(desired response biases), sX), 
# Where X is the desired scenario.


# s1() ===========================================================================

s1 <- function(scale.pResp){
  
  
  fuss1         <- TRUE     # do a follow up
  fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
  #   relative to initial
  
  # Simulate population:
  dat <- simple(n           = n,
                split       = split,
                pSuccess1   = pSuccess1,
                pSuccess2   = pSuccess2,
                pSample     = pSample,
                pResp       = pResp,
                scale.pResp = scale.pResp,
                fus         = fuss1,
                fus.scale   = fus.scales1)
  
  # s1: estimates:
  s1_estimates <-  unique(dat$uns_resp_rate) %>% 
    map_dfr(s1_estimator, x = dat)
  
  # output:
  return(s1_estimates)
}

# s2() ===========================================================================

s2 <- function(){
  
  # s2: Simulating population
  s2 <- mand(n, split, pSuccess1, pSuccess2, pResp)
  
  # s2: Analysis
  s2_estimates <- s2_estimator(s2)
  
  return(s2_estimates)
}

# s3() ===========================================================================

s3 <- function(scale.pResp){
  
  # s3: Simulating population 
  dat <- voluntary(n           = n,
                   split       = split,
                   pSuccess1   = pSuccess1,
                   pSuccess2   = pSuccess2,
                   pResp       = pResp,
                   scale.pResp = scale.pResp)
  
  # s3: Analysis
  s3_estimates <-  unique(dat$uns_resp_rate) %>% 
    map_dfr(s3_estimator, x = dat)
  
  return(s3_estimates)
  
}

# s4() ===========================================================================

s4 <- function(scale.pResp){
  
  # s4: Simulating population
  dat <- voluntary(n           = n,
                   split       = split,
                   pSuccess1   = pSuccess1,
                   pSuccess2   = pSuccess2,
                   pResp       = pResp,
                   scale.pResp = scale.pResp)
  
  # s4: Analysis 
  s4_estimates <-  unique(dat$uns_resp_rate) %>% 
    map_dfr(s4_estimator, x = dat)
  
  return(s4_estimates)
}

# s5() ===========================================================================

s5 <- function(scale.pResp){
  
  # s5: Simulating population:
  fuss5         <- TRUE     # do a follow-up this time.
  fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
  fus.scales5   <- 1        # when 1, equally as likely to respond to follow up 
  #                               as were to originally self-report. 
  
  dat <- voluntary(n             = n,
                   split         = split,
                   pSuccess1     = pSuccess1,
                   pSuccess2     = pSuccess2,
                   pResp         = pResp,
                   scale.pResp   = scale.pResp,
                   fus           = fuss5,
                   fus.pSample   = fus.pSamples5,
                   fus.scale     = fus.scales5)
  
  # s5: Analysis 
  s5_estimates <-  unique(dat$uns_resp_rate) %>% 
    map_dfr(s5_estimator, x = dat)
  
  return(s5_estimates)
}

# Environment ==================================================================

# Below are variables that will be used in each scenario.
# Each scenario will have other arguments specific to the scenario, which are
# defined in their respective scenario functions. (i.e. s1(), s2(), s3()....)

n           <- 10000    # pop. size
split       <- 0.7      # proportion of hunters being in group 1
pSuccess1   <- 0.25     # probability of harvest for group 1
pSuccess2   <- 0.65     # probability of harvest for group 2, 37% avg.
pSample     <- 0.5      # probability a single hunter is sampled
pResp       <- 
  seq(0.2, 1, 0.1)    # probabilities for response to survey. 
# note, for simple() and voluntary() this applies to
# unsuccessful hunters.