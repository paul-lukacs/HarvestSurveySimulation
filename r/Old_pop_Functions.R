########## Population generator function for simple random samples ##########

# The below function simulates a population.
# It simulates harvest (after pop is split into two evenly sized groups of differing success rates), If they were selected for a survey, and if they responded, including follow up surveys
# The function returns a list with 3 + X elements
# 1st element is the total population size (a number)
# 2nd element is the true harvest of entire population (also a number)
# 3rd element is a dataframe of the complete population, but with blank survey columns.
# X more elements for as many levels of response simulated there are.

# ARGUMENTS:
# n         = pop size
# pSuccess1 = prob. of hunter group 1 harvesting
# pSuccess2 = prob. of hunter group 2 harvesting. Is equal to pSuccess1 by default (for scenarios where all hunters are equally likely to harvest)
# pSample   = probability a hunter is sampled for original survey
# suc       = a probability, or seq() of probabilities that dictates response rates of successful hunters
# uns       = 0 by default, but a percentage to scale suc by. e.g. if you want unsuccessful hunters to be 20% less likely to respond, input 20 to argument. 
# FUS       = FALSE by default but can be TRUE to conduct a follow up survey of nonrespondents
# FUSscale  = Scales original response probs. Can be interpreted as, "hunters selected for follow up surveys respond "FUSscale"% less than they would have to the original survey. 

popgen <- function (n, pSuccess1, pSuccess2 = pSuccess1, pSample, suc, uns = 0, FUS = FALSE, FUSscale = 0){
  
  # First some quick checks to make sure arguments were inputted correctly:
  if (length (suc) > 10|| length (suc) < 1){
    stop("\n\tLength of 'suc' too long or short (must be of length 1-10)", .call = FALSE)
  }
  if (FUSscale < 0 || FUSscale >= 100){
    stop("FUSscale must be 0-100. Cannot equal 100")
  }
  if (class(FUS) != "logical"){
    stop("FUS must be T/F")
  }
  if (pSample <= 0 || pSample > 1){
    stop("pSample must be larger than 0 and less than or equal to 1")
  }
  if (pSuccess1 <= 0 || pSuccess1 > 1){
    stop("pSuccess1 must be larger than 0 and less than or equal to 1")
  }
  if (pSuccess2 <= 0 || pSuccess2 > 1){
    stop("pSuccess2 must be larger than 0 and less than or equal to 1")
  }
  
  if (uns < 0 || uns >= 100){
    stop("uns must be 0-100. Cannot equal 100")
  }
  
  # Create initial matrix to eventually hold all population data
  pop <- matrix (NA, nrow = n, ncol = 4)		                # Creating initial matrix with row length n.
  colnames(pop) <- c("Group", "Harvest", "Sampled", "Response")
  
  if (FUS == TRUE){                                         # Make columns for follow up survey
    FUSmat <- matrix(NA, nrow = n, ncol = 1)
    colnames(FUSmat) <- "FUS"                               # Name column
    pop <- cbind(pop, FUSmat)                               # Add it
  }
  
  # Start filling in matrix "pop"
  
  pop [ , 1] <- rbinom (n, 1, 0.5)                          # Split population into two equal groups
  
  pop [ , 2] <- ifelse (pop [ , 1] == 1, 
                        rbinom (length(pop), 1, pSuccess1), 
                        rbinom (length(pop), 1, pSuccess2)) # Simulate "Harvest" for both groups
  
  pop [ , 3] <- rbinom (n, 1, pSample)                      # Sampled or not?
  sampled <- subset (pop, pop [ , 3] == TRUE)               # Creating new matrix of sampled hunters only
  
  
  # Create an array that copies sampled population matrix by length(suc) times.
  init.surv <- array(sampled, dim = c(nrow(sampled), ncol(sampled), length(suc)))  
  
  colnames(init.surv) <- colnames(pop)
  dimnames(init.surv)[[3]] <- suc                         # Each matrix will be named by respective response probability in argument "suc"
  
  # Now to fill in the array's "response" column:
  y <- sampled [ ,2]                                      # Extracting whether sampled hunter had successful harvest or not
  uns <- suc*(1-(uns/100))                                # Scaling rates of response for unsuccessful hunters by percentage specified.
  
  for (j in 1:nrow(init.surv)){
    if (y [j] == 1){                                      # If hunter had successful harvest, do this, otherwise go to else statement.
      init.surv [j, 4, ] <- rbinom (length(suc), 1, suc)  # use vectorized rbinom to generate a vector of trials and save the whole vector to samp[j,4, ] (note the third subscript is left out to allow the vector to be added).                                        
    }
    else {
      init.surv [j, 4, ] <- rbinom (length(uns), 1, uns)  # Now do the same for unsuccessful hunters.
    }
  }
  
  # Conduct any follow up surveys:
  
  if (FUS == TRUE){
      suc <- suc*(1-(FUSscale/100))                                         # Scale original response probs.
      uns <- uns*(1-(FUSscale/100))
      for (i in 1:(nrow(init.surv))){ 
        init.surv[i, 5, ] <- ifelse (init.surv[i, 4, ] == FALSE,            # If hunter didn't respond to initial survey....
                                       ifelse(init.surv[i, 2, ] == TRUE,    # (Column 2 is harvest)
                                              rbinom(length(suc), 1, suc),  # And they did harvest, simulate response for FUS
                                              rbinom(length(uns), 1, uns)), # And they didn't harvest, simulate response for FUS
                                       1)                                   # Otherwise, they already responded.                  
      }
  }

  # Creating Outputs:
  df.list <- apply(init.surv, 3, as.data.frame)       # Make the survey matrices into a list of dataframes
  TrueHvst <- sum (pop [ , 2])                        # True harvest of entire pop.
  list <- list(n, TrueHvst, as.data.frame(pop))       # Combine pop. size, number of harvests, and entire population
  names(list) <- c("n", "true harvest", "complete pop")
  biglist <- c(list, df.list)                         # Combine the two lists
  return(biglist)                                                                
}

########## Population function for mandatory scenarios. ##########

# This function is exactly the same as popgen(), except for a few things:
# It assumes mandatory reporting for successful hunters only
# This means "sampled" column is not simulated, and instead subsets hunters who harvested to simulate response from.
# "uns" argument is removed, as unsuccessful hunters are theoretically not responding.
# There are also no arguments for follow up surveys.

mandpopgen <- function (n, pSuccess1, pSuccess2 = pSuccess1, suc){
  
  # First some quick checks to make sure arguments were inputted correctly:
  if (length (suc) > 10 || length (suc) < 1){
    stop("Length of 'suc' too long or short (must be of length 1-10)")
  }
  if (pSuccess1 <= 0 || pSuccess1 > 1){
    stop("pSuccess1 must be larger than 0 and less than or equal to 1")
  }
  if (pSuccess2 <= 0 || pSuccess2 > 1){
    stop("pSuccess2 must be larger than 0 and less than or equal to 1")
  }
  
  # Create initial matrix to eventually hold all population data
  pop <- matrix (NA, nrow = n, ncol = 3)		                # Creating initial matrix with row length n.
  colnames(pop) <- c("Group", "Harvest", "Response")
  
  
  # Start filling in matrix "pop"
  pop [ , 1] <- rbinom (n, 1, 0.5)                           # Split population into two equal groups
  
  pop [ , 2] <- ifelse (pop [ , 1] == 1, 
                        rbinom (length(pop), 1, pSuccess1), 
                        rbinom (length(pop), 1, pSuccess2)) # Simulate "Harvest" for both groups
  
  sampled <- subset (pop, pop [ , 2] == TRUE)               # Creating new matrix conatining only hunters that harvested
  
  
  # Create array that will hold hunter response sims.
  init.surv <- array(sampled, dim = c(nrow(sampled), ncol(sampled), length(suc)))  
  
  colnames(init.surv) <- colnames(pop)
  dimnames(init.surv)[[3]] <- suc                       # Third dimensions will be named by their respective responding rates.
  
  # Now to fill in the array's "response" column:
  for (j in 1:nrow(init.surv)){                                     
    init.surv [j, 3, ] <- rbinom (length(suc), 1, suc)  # use vectorized rbinom to generate a vector of trials and save the whole vector to samp[j,4, ] (note the third subscript is left out to allow the vector to be added).                                        
  }
  
  # Creating Outputs:
  df.list <- apply(init.surv, 3, as.data.frame)       # Make the survey matrices into a list of dataframes
  TrueHvst <- sum (pop [ , 2])                        # True harvest of entire pop.
  list <- list(n, TrueHvst, as.data.frame(pop))       # Combine pop. size, number of harvests, and entire population
  names(list) <- c("n", "true harvest", "complete pop")
  biglist <- c(list, df.list)                         # Combine the two lists
  return(biglist)                                                                
}

########## Population generator function for voluntary reporting scenarios ##########

# no more pSample argument as there is no original sample.
# Removed FUSscale argument
# FUSprob = an added argument that is the prob. of a hunter that didn't voluntarily respond being sampled for a follow up
# Follow up surveys are simulated where all hunters are equally likely to respond, based off argument "suc"
# For example, an original voluntary reporting rate for a successful hunter of 0.3 means a responding rate of 0.3 for ALL hunters in the follow up. 

volpopgen <- function (n, pSuccess1, pSuccess2 = pSuccess1, suc, uns = 0, FUS = FALSE, FUSprob = 0){
  
  # First some quick checks to make sure arguments were inputted correctly:
  if (length (suc) > 10|| length (suc) < 1){
    stop("Length of 'suc' too long or short (must be length 1-10)")
  }
  if (FUS == TRUE && FUSprob == 0){
    stop("FUSprob must be specified if FUS is TRUE")
  }
  if (class(FUS) != "logical"){
    stop("arg. FUS must be T/F")
  }
  if (pSuccess1 <= 0 || pSuccess1 > 1){
    stop("pSuccess1 must be larger than 0 and less than or equal to 1")
  }
  if (pSuccess2 <= 0 || pSuccess2 > 1){
    stop("pSuccess2 must be larger than 0 and less than or equal to 1")
  }
  
  if (uns < 0 || uns >= 100){
    stop("uns must be a percentage. Cannot equal 100")
  }
  
  # Create initial matrix to eventually hold all population data
  pop <- matrix (NA, nrow = n, ncol = 3)		                # Creating initial matrix with row length n.
  colnames(pop) <- c("Group", "Harvest", "Response")
  
  if (FUS == TRUE){                                         # Make columns for follow up survey
    FUSmat <- matrix(NA, nrow = n, ncol = 2)
    colnames(FUSmat) <- c("Sample", "FUS")                  # Name columns
    pop <- cbind(pop, FUSmat)                               # Add them
  }
  
  # Start filling in matrix "pop"
  pop [ , 1] <- rbinom (n, 1, 0.5)                          # Split population into two equal groups
  
  pop [ , 2] <- ifelse (pop [ , 1] == 1, 
                        rbinom (length(pop), 1, pSuccess1), 
                        rbinom (length(pop), 1, pSuccess2)) # Simulate "Harvest" for both groups
  
  
  # Simulate hunter voluntary reporting.
  init.surv <- array(pop, dim = c(nrow(pop), ncol(pop), length(suc)))  
  
  colnames(init.surv) <- colnames(pop)
  dimnames(init.surv)[[3]] <- suc                         # Each matrix will be named by respective response probability in argument "suc"
  
  y <- pop [ ,2]                                          # Extracting whether sampled hunter had successful harvest or not
  uns <- suc*(1-(uns/100))                                # Scaling rates of response for unsuccessful hunters by percentage specified.
  
  for (j in 1:nrow(init.surv)){
    if (y [j] == 1){                                      # If hunter had successful harvest, do this, otherwise go to else statement.
      init.surv [j, 3, ] <- rbinom (length(suc), 1, suc)  # Simulate reporting for successful hunter                                     
    }
    else {
      init.surv [j, 3, ] <- rbinom (length(uns), 1, uns)  # Now do the same for unsuccessful hunters.
    }
  }
  
  # Follow up survey:
  if (FUS == TRUE){
    for (i in 1:(nrow(init.surv))){ 
      init.surv[i, 4, ] <- ifelse (init.surv[i, 3, ] == FALSE,           # If hunter didn't voluntarily report
                                   rbinom(length(suc), 1, FUSprob),      # Simulate sampled or not for FUS
                                   0)                                    # Otherwise, they already responded and shouldn't be sampled
      
      init.surv[i, 5, ] <- ifelse (init.surv[i, 4, ] == TRUE,            # If nonresponding hunter was selected for FUS
                                   rbinom(length(suc), 1, suc),          # simulate response for FUS
                                   0)                                    # Otherwise, they weren't selected.
      
      # If they responded the first time, copy that to the second survey responses
      init.surv[i, 5, ] <- ifelse (init.surv[i, 3, ] == TRUE,
                                   1,
                                   init.surv[i, 5, ])
    }
  }
  
  # Creating Outputs:
  df.list <- apply(init.surv, 3, as.data.frame)       # Make the survey matrices into a list of dataframes
  TrueHvst <- sum (pop [ , 2])                        # True harvest of entire pop.
  list <- list(n, TrueHvst, as.data.frame(pop))       # Combine pop. size, number of harvests, and entire population
  names(list) <- c("n", "true harvest", "complete pop")
  biglist <- c(list, df.list)                         # Combine the two lists
  return(biglist)                                                                
}
########## SCENARIO 1: Simple random sample w/ follow up ##########

# population of 10,000
# 2 groups of hunters, 1st group prob of harvest =0.4, 2nd group =0.2, for avg. harvest rate of 0.3
# sample ~ 55% of population
# simulate response where successful and unsuccessful hunters are equally likely to respond from 20-100% by 10% increments.
# And simulate follow up survey where nonrespondents are half as likely to respond as they were to the orig. survey.
s1.equal <- popgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, pSample = 0.55, suc = seq(0.2, 1, 0.1), FUS = TRUE, FUSscale = 50)

s1.equal[["n"]]
head(s1.equal[["complete pop"]])
s1.equal[["true harvest"]]
apply(s1.equal[["0.5"]], 2, mean)

# Same as above, but unsuccessful hunters are 20% less likely to respond than successful hunters.
s1.bias20 <- popgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, pSample = 0.55, suc = seq(0.2, 1, 0.1), uns = 20, FUS = T, FUSscale = 50)

apply(s1.bias20[["0.5"]], 2, mean)

########## SCENARIO 2: Voluntary self report, w/ follow up ##########

# 20% less likely to have a response from an unsuccessful hunter voluntarily. All hunters equally likely (based on argument "suc") to respond to a follow up survey given to 20% of nonrespondents.
s2.bias20 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 20, FUS = T, FUSprob = 0.2)
s2.bias20[["0.5"]]
apply(s2.bias20[["0.3"]], 2, mean)

#30% "
s2.bias30 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 30, FUS = T, FUSprob = 0.2)
apply(s2.bias30[["0.3"]], 2, mean)

#40% "
s2.bias40 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 40, FUS = T, FUSprob = 0.2)
apply(s2.bias40[["0.3"]], 2, mean)

########## SCENARIO 3: Voluntary self report, w/o follow up ##########

# Same scenarios as scenario 2, just without follow ups
#20% reporting bias
s3.bias20 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 20)
s3.bias20[["0.3"]]
apply(s3.bias20[["0.3"]], 2, mean)

#30% bias
s3.bias30 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 30)
apply(s3.bias30[["0.3"]], 2, mean)

#40% bias
s3.bias40 <- volpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 0.5, 0.1), uns = 40)
apply(s3.bias40[["0.3"]], 2, mean)

########## SCENARIO 4: Mandatory reporting for successful hunters ##########

# Population of 10k, with varying levels of reporting rates for successful hunters only. Unsuccessful hunters not reporting.
s4 <- mandpopgen(n = 10000, pSuccess1 = 0.4, pSuccess2 = 0.2, suc = seq(0.1, 1, 0.1)) 
apply(s4[["0.5"]], 2, mean)

########## Analyzing data ##########




)