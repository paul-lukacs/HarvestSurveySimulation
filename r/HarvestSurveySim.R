########## Population generator function, with examples ##########

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
# suc       = a seq() that dictates response rates of successful hunters
# uns       = same as "suc" by default, but is also seq() of response rates for unsuccessful hunters if specified
# FUS       = 0 by default, but can be specified to indicate how many follow ups to conduct
# FUSscale  = scales original "suc" and "uns" arguments (i.e. suc * FUSscale). Default is 1, meaning if not specified everyone re-sampled will respond w/ the same prob. as originally defined in suc and uns. It rescales for each consecutive follow up. 

popgen <- function (n, pSuccess1, pSuccess2 = pSuccess1, pSample, suc, uns = suc, FUS = 0, FUSscale = 1){
  
  # First some quick checks to make sure arguments were inputted correctly:
  if (length (suc) > 10 || length (uns) >10 || length (suc) < 2 || length (uns) < 2 ){
    stop("Length of 'suc' or 'uns' too long or short (must be between 2 and 10)")
  }
  if (FUSscale <= 0 || FUSscale > 1){
    stop("FUSscale must be larger than 0 and less than or equal to 1")
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
  
  if (length (suc) != length (uns)){
    stop ("length of 'suc' & 'uns' must be of equal length")
  }
  
  # Create initial matrix to eventually hold all population data
  pop <- matrix (NA, nrow = n, ncol = 4)		                # Creating initial matrix with row length n.
  columns <- c("Group", "Harvest", "Sampled", "Response")
  colnames(pop) <- columns
  
  if (FUS > 0){                                             # Make columns for any follow up surveys
    FUSmat <- matrix(NA, nrow = n, ncol = FUS)
    colnames(FUSmat) <- paste ("FUS", seq(1, FUS, 1))       # Name them
    pop <- cbind(pop, FUSmat)                               # Add them
  }
  
  # Start filling in matrix "pop"
  z <- row(pop)                                             # Extracting row numbers from matrix "pop"
  z <- z[ ,1]                                               # Turning it into a vector containing row numbers
  
  pop [ , 1] <- ifelse (z <= n/2,
                        1,
                        0)                                  # Fill in "Group" column with 1st half of pop. = 1 and 2nd half = 0
  
  pop [ , 2] <- ifelse (pop [ , 1] == 1, 
                        rbinom (length(pop), 1, pSuccess1), 
                        rbinom (length(pop), 1, pSuccess2)) # Simulate "Harvest" for both groups
  
  pop [ , 3] <- rbinom (length(z), 1, pSample)              # Sampled or not?
  sampled <- subset (pop, pop [ , 3] == TRUE)               # Creating new matrix of sampled hunters only
  
  
  # Simulate hunter response to initial surveys.
  init.surv <- array(sampled, dim = c(nrow(sampled), ncol(sampled), length(suc)))  
  
  colnames(init.surv) <- colnames(pop)
  init.surv.names <- paste(suc, uns)                  # Adding reporting prob. for easier recall. 
  dimnames(init.surv)[[3]] <- init.surv.names         # Third dimensions will be named by their respective responding rates with 1st number (cont.)
  # being prob. succ. hunter responds and 2nd number being prob. unsucc. hunter responds (e.g. "0.8 0.6")
  
  
  
  # Now to fill in the array's "response" column:
  y <- sampled [ ,2]                                  # Extracting whether sampled hunter had successful harvest or not
  
  for (j in 1:nrow(init.surv)){
    if (y [j] == 1){                                      # If hunter had successful harvest, do this, otherwise go to else statement.
      init.surv [j, 4, ] <- rbinom (length(suc), 1, suc)  # use vectorized rbinom to generate a vector of trials and save the whole vector to samp[j,4, ] (note the third subscript is left out to allow the vector to be added).                                        
    }
    else {
      init.surv [j, 4, ] <- rbinom (length(uns), 1, uns)  # Now do the same for unsuccessful hunters.
    }
  }
  
  # Conduct any follow up surveys:
  
  if (FUS > 0){                                                                 # If follow up surveys are to be done, do 'em
    for (j in 1:FUS-1){                                                         # Ok, confusing, but starts at 0 (NOT 1!) and ends at FUS-1
      suc <- suc*FUSscale                                                       # For each follow up survey, the next one will be less likely to be responded to.
      uns <- uns*FUSscale
      for (i in 1:(nrow(init.surv))){                       
        init.surv[i, 5+j, ] <- ifelse (init.surv[i, 4+j, ] == FALSE,
                                       ifelse(init.surv[i, 2, ] == TRUE,
                                              rbinom(length(suc), 1, suc),
                                              rbinom(length(uns), 1, uns)),
                                       1)                                       
        # Ok, this whole loop says: 
        # Starting from line with first ifelse(): If the most recent survey was not responded to by hunter i, (initial survey is column 4, and first loop j = 0)
        # next line: and if hunter i harvested, 
        # next line: re-evaluate if they answered this time around based off new scaled probabilities, 
        # next line: If they didn't respond to any of the previous surveys, but didn't harvest, will they respond this time?
        # next line: If they already responded to a previous survey don't survey them again and consider them responded. 
        # then it loops back to the top for loop, rescaling response rates and doing that again and again until all FUS are done
      }
    }}
  
  # Creating Outputs:
  df.list <- apply(init.surv, 3, as.data.frame)       # Make the survey matrices into a list of dataframes
  TrueHvst <- sum (pop [ , 2])                        # True harvest of entire pop.
  list <- list(n, TrueHvst, as.data.frame(pop))       # Combine pop. size, number of harvests, and entire population
  names(list) <- c("n", "true harvest", "complete pop")
  biglist <- c(list, df.list)                         # Combine the two lists
  return(biglist)                                                                
}

####################### Examples of functions ##############################
########## Large pop. w/ successful and unsuccessful hunters equally likely to respond, no follow up surveys ##########

# Simulate a population of 10,000; split into 2 groups of hunters with different rates of harvest (0.4 and 0.2 for an average harvest rate of 30%) and all have a probability of being sampled = 0.5.
Lpop <- popgen (n = 10000, 
                pSuccess1 = 0.40, 
                pSuccess2 = 0.20, 
                pSample = 0.5,
                suc = seq (.2, 1, 0.1))  

Lpop[["n"]]                   # Reports Total population size
Lpop[["true harvest"]]        # Reports total harvest of entire pop.
head(Lpop[["complete pop"]])  # Reports dataframe of complete pop w/o survey columns filled in
head(Lpop[["0.5 0.5"]], 12)   # Reports survey outcomes when both successful and unsuccessful hunters responded with prob. 0.5
                              # Note, only sampled population contained in those dataframes
apply(Lpop[["0.5 0.5"]], 2,  mean) # avg. of columns

########## Medium pop. w/ successful and unsuccessful hunters not equally likely to respond, no follow up surveys ###########

# Simulate a population of 1,000; split into groups of hunters with different rates of harvest (0.35 or 0.25 for an average harvest rate of 30%) and all have a probability of being sampled = 0.5, but successful hunters are 20% more likely to respond to surveys. 

Mpop.NRbias <- popgen (n = 1000, 
                       pSuccess1 = 0.35, 
                       pSuccess2 = 0.25, 
                       pSample = 0.5,
                       suc = seq (0.3, 0.9, 0.1), 
                       uns = seq (0.1, 0.7, 0.1))  

Mpop.NRbias[["n"]]    
Mpop.NRbias[["true harvest"]]       
head(Mpop.NRbias[["0.4 0.2"]], 12)    # Reports simulation when successful hunters responded @ 40% and unsuccessful @ 20%
apply(Mpop.NRbias[["0.6 0.4"]], 2, mean)

########## Medium pop. w/ unequal response rates and 3 follow up surveys ##########

# A population of 1,000 ........., with 3 follow up surveys conducted, each consecutive follow up survey being 50% less likely to hear a response from someone who didn't respond to any of the previous ones. 

Lpop.NRbias.3FUS <- popgen (n = 1000,
                            pSuccess1 = 0.4,
                            pSuccess2 = 0.2,
                            pSample = 0.5,
                            suc = seq (0.3, 0.9, 0.1),
                            uns = seq (0.1, 0.7, 0.1),
                            FUS = 3,
                            FUSscale = 0.5)

Lpop.NRbias.3FUS[["n"]]
head(Lpop.NRbias.3FUS[["0.3 0.1"]], 20)
head(Lpop.NRbias.3FUS[["0.7 0.5"]], 12)
head(Lpop.NRbias.3FUS[["complete pop"]])
apply(Lpop.NRbias.3FUS[["0.6 0.4"]], 2, mean)
