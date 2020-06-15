library(tidyverse) ; library(survey)

# Scenario 1 ==================================================================
# Simple random sample, with follow up survey. Both estimates post-stratified by
# "group" variable then combined for a complete estimate.

# Change first argument in map_dfr() to change response bias

s1_estimates <- map_dfr(c(1, 1.1, 1.2, 1.3, 1.4), s1)

# Scenario 2 ===================================================================
# A scenario where successful hunters are mandated to report. 
# Estimates assume 100% reporting for successful hunters, i.e. the estimate is
# equal to reported harvests.

s2_estimates <- s2()

# Scenario 3 ===================================================================
# This scenario will be voluntary reporting (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.

s3_estimates <- map_dfr(c(1, 1.1, 1.2, 1.3, 1.4), s3)

# Scenario 4 ===================================================================
# Voluntary reporting w/o follow-up, but with post-stratification by "group" 

s4_estimates <- map_dfr(c(1, 1.1, 1.2, 1.3, 1.4), s4)

# Scenario 5 ===================================================================
# A voluntary survey, with a simple random sample follow up of nonrepondents.
# both stages post-stratified by "group".

s5_estimates <- map_dfr(c(1, 1.2, 1.2, 1.3, 1.4), s5)