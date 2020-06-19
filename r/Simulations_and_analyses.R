library(tidyverse) ; library(survey)

# Scenario 1 ==================================================================
# Simple random sample, with follow up survey. Both estimates post-stratified by
# "group" variable then combined for a complete estimate.

# The arguments to s1() are the desired level of response bias, and the number
# of times to re-simulate each response bias.
s1_estimates <- s1(respbias = seq(1, 1.4, 0.1), times = times) 

# Summarise all those iterations:
s1_averages <- s1_estimates %>% group_by(resp_bias) %>% 
  summarise(
    mean_relative_bias = mean(relative_bias)
  )

# Scenario 2 ===================================================================
# A scenario where successful hunters are mandated to report. 
# Estimates assume 100% reporting for successful hunters, i.e. the estimate is
# equal to reported harvests.

s2_estimates <- s2(times)

# Summarise all those iterations:
s2_averages <- s2_estimates %>% group_by(resp_bias) %>% 
  summarise(
    mean_relative_bias = mean(relative_bias)
  )


# Scenario 3 ===================================================================
# This scenario will be voluntary reporting (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.

s3_estimates <- s3(respbias = seq(1, 1.4, 0.1), times = times)

# Summarise all those iterations:
s3_averages <- s3_estimates %>% group_by(resp_bias) %>% 
  summarise(
    mean_relative_bias = mean(relative_bias)
  )

# Scenario 4 ===================================================================
# Voluntary reporting w/o follow-up, but with post-stratification by "group" 

s4_estimates <- s4(respbias = seq(1, 1.4, 0.1), times = times)

# Summarise all those iterations:
s4_averages <- s4_estimates %>% group_by(resp_bias) %>% 
  summarise(
    mean_relative_bias = mean(relative_bias)
  )

# Scenario 5 ===================================================================
# A voluntary survey, with a simple random sample follow up of nonrepondents.
# both stages post-stratified by "group".

s5_estimates <- s5(respbias = seq(1, 1.4, 0.1), times = times)

# Summarise all those iterations:
s5_averages <- s5_estimates %>% group_by(resp_bias) %>% 
  summarise(
    mean_relative_bias = mean(relative_bias)
  )

