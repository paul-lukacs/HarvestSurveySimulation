library(dplyr) ; library(purrr)

# simple() function ============================================================

# Simulates a simple random sample of n hunters
# Returns a single tibble
# tibble will be of length n*length(pResp),
#   where pResp contains a sequence of levels of response to be simulated
#   e.g. if n = 10 and pResp = c(0.5, 0.6) the length of tibble will = 10*2 = 20
# For values in columns pop_size:sample, values will be repeated every nth row
#   i.e. the same population is used for all levels of different response rates.
# Column "fus_resp" populates NAs for hunters who already responded.

# Usage and Arguments ==========================================================
# 
# simple(n, 
#        split = 0.7, 
#        pSuccess1, 
#        pSuccess2 = pSuccess1, 
#        pSample,
#        pResp, 
#        scale.pResp = 1, 
#        fus = FALSE, 
#        fus.scale = 1)
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

# simple() function body =======================================================

simple <- function(n, split = 0.7, pSuccess1, pSuccess2 = pSuccess1, pSample,
                   pResp, scale.pResp = 1, fus = FALSE, fus.scale = 1){
  
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
            harvest == 1 & init_resp == 0
            ~ rbinom(n*length(pResp), 1, fus_suc_resp_rate),
            
            harvest == 0 & init_resp == 0
            ~ rbinom(n*length(pResp), 1, fus_uns_resp_rate),
            
            init_resp == 1 
            ~ NA_integer_,
            
            TRUE         
            ~ NA_integer_
          )
      )
  }
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# Example ======================================================================

# A population of 1,000.
# 70% of the pop is in group 1, the other 30% in group 0 (the default)
# Group 1 harvests at a prob. of 0.25
# Group 0 at a prob. of 0.7, for a population avg. harvest rate of 38.5%
# Probability a hunter is sampled is 0.5
# The avg. response for unsuccesful hunters is a sequence from 0.2 : 1 by 0.1
# Successful hunters respond 1.2 times more than unsuccessful
# Simulate follow up survey where hunters respond 50% less than they would have
#   originally.

example <- simple(n = 1000,
                  pSuccess1 = 0.25, 
                  pSuccess2 = 0.7, 
                  pSample = 0.5, 
                  pResp = seq(0.2, 1, 0.1), 
                  scale.pResp = 1.2, 
                  fus = TRUE, 
                  fus.scale = 0.5)


# Summarizing and checking output ==============================================

# Avg. of columns
map_dfr(example, mean, na.rm = TRUE)

# Avg. of harvest by group
example %>% 
  group_by(group) %>% 
  summarise(harvest = mean(harvest))

# Avg. of response by harvest
example %>% 
  group_by(harvest) %>% 
  summarise(actual_response_rate = mean(init_resp),
            actual_followup_rate = mean(fus_resp, na.rm = TRUE))

# mand() function ==============================================================

# Assumes mandatory reporting by successful hunters only.
# Lacks a few columns compared to simple() function:
  # No sampled column.
  # No column for response rate for unsuccessful hunters
  # No columns for any follow up survey variables.

# Arguments ====================================================================

mand (n, 
      split = 0.7, 
      pSuccess1, 
      pSuccess2 = pSuccess1, 
      pResp)

# n             Desired population size.
# split         Proportion of the population that is placed into group 1
# pSuccess1     Probability of hunter in group 1 to harvest
# pSuccess2     Probability of hunter in group 2 to harvest
# pResp         Response probability/probabilities for **SUCCESSFUL** hunters.
#                 as they are the only ones responding.
  
# mand() function body =========================================================

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
          TRUE         ~ NA_integer_
        )
    )
  
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# Example ======================================================================

example <- mand(10,
                pSuccess1 = 0.25,
                pSuccess2 = 0.7,
                pResp = seq(0.2, 1, 0.2))

# voluntary() function =========================================================

# Simulates a voluntary reporting scenario.
# Mostly same variables as simple() function, except takes a random sample for 
  # follow up survey, instead of for initial survey.

# Arguments ====================================================================

# voluntary <- (n,
#               split = 0.7,
#               pSuccess1,
#               pSuccess2 = pSuccess1,
#               pResp,
#               scale.pResp = 1,
#               fus = FALSE,
#               fus.pSample,
#               fus.scale = 1)
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

# voluntary() function body ====================================================
voluntary <- function(n, split = 0.7, pSuccess1, pSuccess2 = pSuccess1,
                      pResp, scale.pResp = 1, fus = FALSE, fus.pSample, fus.scale = 1){
  
  # Function changeto1 =========================================================
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
    if (fus.scale < 1){
      warning(paste0("fus.scale < 1; less likely to respond to follow up",
                     " than to voluntarily report"),
              call. = FALSE)
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
  }
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars.
  full_sim <- full_sim %>% 
    select(pop_size, true_harvest, everything())
  
  return(full_sim)
}

# Example ======================================================================

example <- voluntary(n = 100, 
                     pSuccess1 = 0.25, 
                     pSuccess2 = 0.7, 
                     pResp = seq(0.2, 1, 0.1), 
                     scale.pResp = 1.2, 
                     fus = TRUE, 
                     fus.pSample = 0.2, 
                     fus.scale = 1.2)
