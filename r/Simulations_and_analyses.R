library(tidyverse) ; library(survey)
# ANALYSES FOR NO REPORTING BIAS: ==============================================

# Environment ==================================================================

scale.pResp <- 1

# Scenario 1 ===================================================================

# A simple random sample, with follow up survey and post-stratification by
#   "group" variable at both stages.

# s1: Simulating population ====================================================

# Extra arguments needed for scenario:

fuss1         <- TRUE     # do a follow up
fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
#   relative to initial

s1_nobias <- simple(n           = n,
                    split       = split,
                    pSuccess1   = pSuccess1,
                    pSuccess2   = pSuccess2,
                    pSample     = pSample,
                    pResp       = pResp,
                    scale.pResp = scale.pResp,
                    fus         = fuss1,
                    fus.scale   = fus.scales1)

# s1: Analysis =================================================================

s1_estimates <-  unique(s1_nobias$uns_resp_rate) %>% 
  map_dfr(s1_estimator, x = s1_nobias)


# Scenario 2 ===================================================================

# A scenario where successful hunters are mandated to report. 
# mandatory_est() function assumes 100% reporting for successful hunters.

# s2: Simulating population ====================================================

s2 <- mand(n, split, pSuccess1, pSuccess2, pResp)

# s2: Analysis =================================================================

s2_estimates <- s2_estimator(s2)

# Scenario 3 ===================================================================
#
# This scenario will be a voluntary survey (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.
#
# s3: Simulating population ====================================================

s3_nobias <- voluntary(n           = n,
                       split       = split,
                       pSuccess1   = pSuccess1,
                       pSuccess2   = pSuccess2,
                       pResp       = pResp,
                       scale.pResp = scale.pResp)

# s3: Analysis =================================================================

s3_estimates <-  unique(s3_nobias$uns_resp_rate) %>% 
  map_dfr(s3_estimator, x = s3_nobias)


# Scenario 4 ===================================================================

# Same as scenario 3, but with post-stratification by "group" variable

# s4: Simulating population ====================================================

s4_nobias <- voluntary(n           = n,
                       split       = split,
                       pSuccess1   = pSuccess1,
                       pSuccess2   = pSuccess2,
                       pResp       = pResp,
                       scale.pResp = scale.pResp)

# s4: Analysis =================================================================

s4_estimates <-  unique(s4_nobias$uns_resp_rate) %>% 
  map_dfr(s4_estimator, x = s4_nobias)

# Scenario 5 ===================================================================

# A voluntary survey, with a follow up of nonrepondents.
# both stages post-stratified by "group".

# s5: Simulating population ====================================================

fuss5         <- TRUE     # do a follow-up this time.
fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
fus.scales5   <- 1        # when 1, equally as likely to respond to follow up as 
#   were to originally self-report. 

s5_nobias <- voluntary(n             = n,
                       split         = split,
                       pSuccess1     = pSuccess1,
                       pSuccess2     = pSuccess2,
                       pResp         = pResp,
                       scale.pResp   = scale.pResp,
                       fus           = fuss5,
                       fus.pSample   = fus.pSamples5,
                       fus.scale     = fus.scales5)

# s5: Analysis =================================================================

s5_estimates <-  unique(s5_nobias$uns_resp_rate) %>% 
  map_dfr(s5_estimator, x = s5_nobias)

# ANALYSES FOR 1.1 REPORTING BIAS ==============================================
# Environment ==================================================================

scale.pResp <- 1.1

# Scenario 1 ===================================================================

# A simple random sample, with follow up survey and post-stratification by
#   "group" variable at both stages.

# s1: Simulating population ====================================================

# Extra arguments needed for scenario:
fuss1         <- TRUE     # do a follow up
fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
#                             relative to initial

s1_1.1bias <- simple(n           = n,
                     split       = split,
                     pSuccess1   = pSuccess1,
                     pSuccess2   = pSuccess2,
                     pSample     = pSample,
                     pResp       = pResp,
                     scale.pResp = scale.pResp,
                     fus         = fuss1,
                     fus.scale   = fus.scales1)

# s1: Analysis =================================================================

s1_estimates <- bind_rows(
  s1_estimates, 
  (unique(s1_1.1bias$uns_resp_rate) %>% map_dfr(s1_estimator, x = s1_1.1bias))
)

# Scenario 3 ===================================================================
#
# This scenario will be a voluntary survey (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.
#
# s3: Simulating population ====================================================

s3_1.1bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s3: Analysis =================================================================

s3_estimates <- bind_rows(
  s3_estimates, 
  (unique(s3_1.1bias$uns_resp_rate) %>% map_dfr(s3_estimator, x = s3_1.1bias))
)



# Scenario 4 ===================================================================

# Same as scenario 3, but with post-stratification by "group" variable

# s4: Simulating population ====================================================

s4_1.1bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s4: Analysis =================================================================

s4_estimates <- bind_rows(
  s4_estimates, 
  (unique(s4_1.1bias$uns_resp_rate) %>% map_dfr(s4_estimator, x = s4_1.1bias))
)

# Scenario 5 ===================================================================

# A voluntary survey, with a follow up of nonrepondents.
# both stages post-stratified by "group".

# s5: Simulating population ====================================================

fuss5         <- TRUE     # do a follow-up this time.
fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
fus.scales5   <- 1        # when 1, equally as likely to respond to follow up as 
#   were to originally self-report. 

s5_1.1bias <- voluntary(n             = n,
                        split         = split,
                        pSuccess1     = pSuccess1,
                        pSuccess2     = pSuccess2,
                        pResp         = pResp,
                        scale.pResp   = scale.pResp,
                        fus           = fuss5,
                        fus.pSample   = fus.pSamples5,
                        fus.scale     = fus.scales5)

# s5: Analysis =================================================================

s5_estimates <- bind_rows(
  s5_estimates, 
  (unique(s5_1.1bias$uns_resp_rate) %>% map_dfr(s5_estimator, x = s5_1.1bias))
)

# ANALYSES FOR 1.2 REPORTING BIAS ==============================================
# Environment ==================================================================

scale.pResp <- 1.2

# Scenario 1 ===================================================================

# A simple random sample, with follow up survey and post-stratification by
#   "group" variable at both stages.

# s1: Simulating population ====================================================

# Extra arguments needed for scenario:
fuss1         <- TRUE     # do a follow up
fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
#                             relative to initial

s1_1.2bias <- simple(n           = n,
                     split       = split,
                     pSuccess1   = pSuccess1,
                     pSuccess2   = pSuccess2,
                     pSample     = pSample,
                     pResp       = pResp,
                     scale.pResp = scale.pResp,
                     fus         = fuss1,
                     fus.scale   = fus.scales1)

# s1: Analysis =================================================================

s1_estimates <-  bind_rows(
  s1_estimates, 
  (unique(s1_1.2bias$uns_resp_rate) %>% map_dfr(s1_estimator, x = s1_1.2bias))
)

# Scenario 3 ===================================================================
#
# This scenario will be a voluntary survey (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.
#
# s3: Simulating population ====================================================

s3_1.2bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s3: Analysis =================================================================

s3_estimates <-  bind_rows(
  s3_estimates, 
  (unique(s3_1.2bias$uns_resp_rate) %>% map_dfr(s3_estimator, x = s3_1.2bias))
)

# Scenario 4 ===================================================================

# Same as scenario 3, but with post-stratification by "group" variable

# s4: Simulating population ====================================================

s4_1.2bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s4: Analysis =================================================================

s4_estimates <-  bind_rows(
  s4_estimates, 
  (unique(s4_1.2bias$uns_resp_rate) %>% map_dfr(s4_estimator, x = s4_1.2bias))
)

# Scenario 5 ===================================================================

# A voluntary survey, with a follow up of nonrepondents.
# both stages post-stratified by "group".

# s5: Simulating population ====================================================

fuss5         <- TRUE     # do a follow-up this time.
fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
fus.scales5   <- 1        # when 1, equally as likely to respond to follow up as 
#                             were to originally self-report. 

s5_1.2bias <- voluntary(n             = n,
                        split         = split,
                        pSuccess1     = pSuccess1,
                        pSuccess2     = pSuccess2,
                        pResp         = pResp,
                        scale.pResp   = scale.pResp,
                        fus           = fuss5,
                        fus.pSample   = fus.pSamples5,
                        fus.scale     = fus.scales5)

# s5: Analysis =================================================================

s5_estimates <-  bind_rows(
  s5_estimates, 
  (unique(s5_1.2bias$uns_resp_rate) %>% map_dfr(s5_estimator, x = s5_1.2bias))
)
# ANALYSES FOR 1.3 REPORTING BIAS ==============================================
# Environment ==================================================================

scale.pResp <- 1.3

# Scenario 1 ===================================================================

# A simple random sample, with follow up survey and post-stratification by
#   "group" variable at both stages.

# s1: Simulating population ====================================================

# Extra arguments needed for scenario:
fuss1         <- TRUE     # do a follow up
fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
#                             relative to initial

s1_1.3bias <- simple(n           = n,
                     split       = split,
                     pSuccess1   = pSuccess1,
                     pSuccess2   = pSuccess2,
                     pSample     = pSample,
                     pResp       = pResp,
                     scale.pResp = scale.pResp,
                     fus         = fuss1,
                     fus.scale   = fus.scales1)

# s1: Analysis =================================================================

s1_estimates <- bind_rows(
  s1_estimates,
  (unique(s1_1.3bias$uns_resp_rate) %>% map_dfr(s1_estimator, x = s1_1.3bias))
)

# Scenario 3 ===================================================================
#
# This scenario will be a voluntary survey (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.
#
# s3: Simulating population ====================================================

s3_1.3bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s3: Analysis =================================================================

s3_estimates <- bind_rows(
  s3_estimates,
  (unique(s3_1.3bias$uns_resp_rate) %>% map_dfr(s3_estimator, x = s3_1.3bias))
)

# Scenario 4 ===================================================================

# Same as scenario 3, but with post-stratification by "group" variable

# s4: Simulating population ====================================================

s4_1.3bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s4: Analysis =================================================================

s4_estimates <- bind_rows(
  s4_estimates,
  (unique(s4_1.3bias$uns_resp_rate) %>% map_dfr(s4_estimator, x = s4_1.3bias))
)

# Scenario 5 ===================================================================

# A voluntary survey, with a follow up of nonrepondents.
# both stages post-stratified by "group".

# s5: Simulating population ====================================================

fuss5         <- TRUE     # do a follow-up this time.
fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
fus.scales5   <- 1        # when 1, equally as likely to respond to follow up as 
#   were to originally self-report. 

s5_1.3bias <- voluntary(n             = n,
                        split         = split,
                        pSuccess1     = pSuccess1,
                        pSuccess2     = pSuccess2,
                        pResp         = pResp,
                        scale.pResp   = scale.pResp,
                        fus           = fuss5,
                        fus.pSample   = fus.pSamples5,
                        fus.scale     = fus.scales5)

# s5: Analysis =================================================================

s5_estimates <- bind_rows(
  s5_estimates,
  (unique(s5_1.3bias$uns_resp_rate) %>% map_dfr(s5_estimator, x = s5_1.3bias))
)

# ANALYSES FOR 1.4 REPORTING BIAS =================================================
# Environment ==================================================================

scale.pResp <- 1.4

# Scenario 1 ===================================================================

# A simple random sample, with follow up survey and post-stratification by
#   "group" variable at both stages.

# s1: Simulating population ====================================================

# Extra arguments needed for scenario:
fuss1         <- TRUE     # do a follow up
fus.scales1   <- 0.5      # hunters half as likely to respond to follow up, 
#                             relative to initial

s1_1.4bias <- simple(n           = n,
                     split       = split,
                     pSuccess1   = pSuccess1,
                     pSuccess2   = pSuccess2,
                     pSample     = pSample,
                     pResp       = pResp,
                     scale.pResp = scale.pResp,
                     fus         = fuss1,
                     fus.scale   = fus.scales1)

# s1: Analysis =================================================================

s1_estimates <- bind_rows(
  s1_estimates, 
  (unique(s1_1.4bias$uns_resp_rate) %>% map_dfr(s1_estimator, x = s1_1.4bias))
)

# Scenario 3 ===================================================================
#
# This scenario will be a voluntary survey (can also be thought of as a
# mandatory reporting for all hunters) without follow up, with harvest rate of
# respondents extrapolated to entire population.
#
# s3: Simulating population ====================================================

s3_1.4bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s3: Analysis =================================================================

s3_estimates <- bind_rows(
  s3_estimates, 
  (unique(s3_1.4bias$uns_resp_rate) %>% map_dfr(s3_estimator, x = s3_1.4bias))
)

# Scenario 4 ===================================================================

# Same as scenario 3, but with post-stratification by "group" variable

# s4: Simulating population ====================================================

s4_1.4bias <- voluntary(n           = n,
                        split       = split,
                        pSuccess1   = pSuccess1,
                        pSuccess2   = pSuccess2,
                        pResp       = pResp,
                        scale.pResp = scale.pResp)

# s4: Analysis =================================================================

s4_estimates <- bind_rows(
  s4_estimates, 
  (unique(s4_1.4bias$uns_resp_rate) %>% map_dfr(s4_estimator, x = s4_1.4bias))
)

# Scenario 5 ===================================================================

# A voluntary survey, with a follow up of nonrepondents.
# both stages post-stratified by "group".

# s5: Simulating population ====================================================

fuss5         <- TRUE     # do a follow-up this time.
fus.pSamples5 <- 0.2      # when 0.2, sample 2/10th of non-reporters.
fus.scales5   <- 1        # when 1, equally as likely to respond to follow up as 
#   were to originally self-report. 

s5_1.4bias <- voluntary(n             = n,
                        split         = split,
                        pSuccess1     = pSuccess1,
                        pSuccess2     = pSuccess2,
                        pResp         = pResp,
                        scale.pResp   = scale.pResp,
                        fus           = fuss5,
                        fus.pSample   = fus.pSamples5,
                        fus.scale     = fus.scales5)

# s5: Analysis =================================================================

s5_estimates <- bind_rows(
  s5_estimates, 
  (unique(s5_1.4bias$uns_resp_rate) %>% map_dfr(s5_estimator, x = s5_1.4bias))
)
