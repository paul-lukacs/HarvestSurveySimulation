# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HUNTER HARVEST SURVEY SIMULATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# run if not installed, or not on Version 1.0.0:
# devtools::install_github("peterdonati/hhss")
library(hhss)

# Simulation parameters ========================================================
split <- 0.7                          # Prob of hunter being in group 1
success1 <- 0.3                       # Prob of group 1 hunter harvesting
success2 <- 0.5                       # Avg. harvest of 36% b/w groups
sample <- 0.5                         # Prob of sample in SRS scenarios
resp <- c(0.2, 0.4, 0.6, 0.8, 0.9, 1) # Prob of response
times <- 100                          # repetitions
fol_scale <- 0.7                      # 30% less likely to respond to follow-up
fol_sample <- 0.3                     # sample 30% of non-reporters. 

# Population creations==========================================================
init_10k <- pop(10000, split, success1, success2)
init_50k <- pop(50000, split, success1, success2)
init_100k <- pop(100000, split, success1, success2)

# Surveys ======================================================================
{
# s1: Mandatory for successful, follow up of non-reporters =====================
s1_func <- function(pop){
  mand(pop, resp = resp, fol = T, bias = seq(0.8, 1.4, 0.1),
       fol_sample = fol_sample, fol_scale = fol_scale, times = times)
}

s1 <- list(
  s1_10k = s1_func(init_10k), 
  s1_50k = s1_func(init_50k), 
  s1_100k = s1_func(init_100k)
)

# s2: SRS ======================================================================
s2_func <- function(pop){
  simple(pop, sample = sample, resp = resp, 
         bias = seq(1, 1.4, 0.1), times = times)
  
}

s2 <- list(
  s2_10k = s2_func(init_10k), 
  s2_50k = s2_func(init_50k), 
  s2_100k = s2_func(init_100k)
)

# s3: SRS w/ follow-up =========================================================
s3_func <- function(pop){
  simple(pop, sample = sample, resp = resp, 
         bias = seq(1, 1.4, 0.1), fol = T, fol_scale = fol_scale, 
         times = times)
}

s3 <- list(
  s3_10k = s3_func(init_10k), 
  s3_50k = s3_func(init_50k), 
  s3_100k = s3_func(init_100k)
)

# s4: census ===================================================================
s4_func <- function(pop){
  census(pop, resp = resp, bias = seq(1, 1.4, 0.1), times = times)
}

s4 <- list(
  s4_10k = s4_func(init_10k), 
  s4_50k = s4_func(init_50k), 
  s4_100k = s4_func(init_100k)
)

# s5: census w/ follow-up ======================================================
s5_func <- function(pop){
  census(pop, resp = resp, bias = seq(1, 1.4, 0.1), fol = TRUE, 
      fol_sample = fol_sample, fol_scale = fol_scale, times = times)
}

s5 <- list(
  s5_10k = s5_func(init_10k), 
  s5_50k = s5_func(init_50k), 
  s5_100k = s5_func(init_100k)
)
}

# Estimates ====================================================================
est_list <- function(input, scenario){
  out <- purrr::map(input, est)
  out <- purrr::map(out, dplyr::mutate, scen = scenario)
  names(out) <- paste0(names(input), "_est")
  return(out)
}

{
# s1 ===========================================================================
s1_ests <- est_list(s1, 1)

# s2 ===========================================================================
s2_ests <- est_list(s2, 2)

# s3 ===========================================================================
s3_ests <- est_list(s3, 3)

# s4 ===========================================================================
s4_ests <- est_list(s4, 4)

# s5 ===========================================================================
s5_ests <- est_list(s5, 5)
}
