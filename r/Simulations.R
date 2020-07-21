memory.limit(size = 20000)

# Simulation params ============================================================
n <- 8000                  # pop size
split <- 0.7               # Prob of hunter being in group 1
success1 <- 0.25           # Prob of group 1 hunter harvesting
success0 <- 0.6            # Avg. harvest of 35.5% b/w groups
sample <- 0.5              # Prob of sample in SRS scenarios
resp <- c(0.2, 0.4, 0.6, 0.8, 0.9, 1)   # Prob of response  
bias <- seq(1, 1.4, 0.1)   # Resp. bias for successful hunters. SRS and vol only
times <- 100               # # of times to repeat simulations.

# Create initial pop:
init <- pop(n, split, success1, success0)

# s1: Mandatory reporting ======================================================
s1_pop <- mand(init, resp, times)
s1_est <- est(s1_pop)
MAREplot(s1_est, "1: Mandatory")

# s2: SRS, no follow, no poststrat =============================================
s2_pop <- simple(init, sample, resp, bias, times = times)
s2_est <- est(s2_pop)
MAREplot(s2_est, "2: SRS without follow up")

# s3: SRS, follow, no poststrat ================================================
# 1/2 as likely to respond to follow up survey than to initial
s3_pop <- simple(init, sample, resp, bias, fus = T, fus_scale = 0.5, times)
s3_est <- est(s3_pop)
MAREplot(s3_est, "3: SRS with follow up")

# s4: Voluntary, no follow, no ps ==============================================
s4_pop <- vol(init, resp, bias, times = times)
s4_est <- est(s4_pop)
MAREplot(s4_est, "4: Voluntary without follow up")

# s5: vol, follow, no ps =======================================================
s5_pop <- vol(init, resp, bias, fus = TRUE, fus_scale = 1.2, fus_sample = 0.1,
              times = times)
s5_est <- est(s5_pop)
MAREplot(s5_est, "5: Voluntary with follow up")






