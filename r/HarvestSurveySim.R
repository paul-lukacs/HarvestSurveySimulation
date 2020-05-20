# Hunter Simulation
#
#
#
#
########## Population generator function ######################################

# The below function simulates a population and returns a list with 3 elements. 
# 1st element is a dataframe containing the entire population. 
# 2nd element is a matrix containing sampled hunters only
# 3rd is the number of total harvests by the simulated population. 
# Response is not calculated by this function

# ARGUMENTS:
# n         = pop size
# pSuccess1 = prob. of hunter group 1 harvesting
# pSuccess2 = prob. of hunter group 2 harvesting. Is equal to pSuccess1 by default (for scenarios where all hunters are equally likely to harvest)
# pSample   = probability a hunter is sampled.

popgen <- function (n, pSuccess1, pSuccess2 = pSuccess1, pSample){  
 
  
  pop <- matrix (NA, nrow = n, ncol = 4)		  # Creating initial matrix with row length n.
  colnames(pop) <- c("Group", "Harvest", "Sampled", "Response")
  
  for (i in 1:n){
    
    if (i <= n/2){                            # Splitting pop. in half.
      pop [i, 1] <- 1                         # Assigning first half to one of two harvest groups
    }                                         
    else {
      pop [i, 1] <- 0                         # Assigning second half to other group.
    }
    
      pop [i, 3] <- rbinom (1, 1, pSample)    # Sampled or not?
  }
  
  
  
  for (i in 1:n){
    if (pop [i, 1] == 1) {
      pop [i, 2] <- rbinom (1, 1, pSuccess1)  # If hunter is in group 1, assign harvest based on pSuccess1
    }
    else{
      pop [i, 2] <- rbinom (1, 1, pSuccess2)  # If hunter is in group 2, assign harvest based on pSuccess2, which could be equal to pSuccess1 if argument left blank.
    }
  }
  
  
######## OUTPUTS
  
  
  TrueHvst <- sum (pop [ , 2])                                                # True harvest of entire pop.
  sampled <- subset (pop, pop[ , 3] == TRUE)                                  # Sampled hunters only, into new matrix
  list <- (list(as.data.frame(pop), sampled, TrueHvst))                       # Creating the list with 3 elements
  names(list) <- c("total population", "sampled population", "true harvest")
  return(list)                                                                
}

########## Survey generator function ##########

# This will return an array with each consecutive third dimension being a simulation of (cont.)
# survey response by increasing levels based on the inputs for arguments "suc" and "uns". 
# sequences for these two arguments must be of equal length and be of length 2-10

# ARGUMENTS: 
# "x"   = a list, from popgen() output
# "suc" = a seq() that dictates response rates of successful hunters
# "uns" = same as "suc" by default, but is also seq() of response rates for unsuccessful hunters if specified

survgen <- function (x, suc, uns = suc){
  
  
#### First some quick checks to make sure sequences were inputted correctly:
  
  if (length (suc) > 10 || length (uns) >10 || length (suc) < 2 || length (uns) < 2 ){
    stop("Lengths of sequences too long or short (must be between 2 and 10)")
  }
  
  if (length (suc) != length (uns)){
    stop ("length of 'suc' & 'uns' must be of equal length")
  }
  
  
  
#### Now to create the array with row and column length of sampled hunters, and replicate by as many terms as in length(suc)
  
  samp <- array(x[[2]], dim = c(nrow(x[[2]]), ncol(x[[2]]), length(suc)))  
  
  colnames(samp) <- c("Group", "Harvest", "Sampled", "Response")
  dimnames(samp)[[3]] <- c(as.character(suc))   # Adding reporting %'s for easier recall
  y <- x[[2]][ ,2]                              # Extracting whether hunter had successful harvest or not to be (cont.)
                                                # later evaluated if T or F when assigning response rates below.
  
  
#### Now to fill in the array:
  
  for (j in 1:nrow(samp)){
    if (y [j] == 1){
      for (k in 1:length(suc)){
        samp [j , 4, k] <- rbinom (1, 1, suc[k])  # For all layers of array apply response based off prob. suc[k] (haha)
      }                                           # suc[k] = prob. that a successful hunter responds based on given seq() for argument "suc" at position k
      
    }
    else {
      for (k in 1:length(suc)){
        samp [j , 4, k] <- rbinom (1, 1, uns[k])  # For first layer of array apply response based off prob. uns[k] (prob. that an unsuccessful hunter responds.)
      }
    }
  }
  return(samp)
}
########## SCENARIO 1: Simple Random Sample by phone w/ follow-up ###########################

########## Example of functions: Large pop. w/ successful and unsuccessful hunters equally likely to report ##########

# Simulate a population of 10,000; split into groups of hunters with different rates of harvest (0.4 or 0.2) which are split evenly (for an average harvest rate of 30%) and all have a probability of being sampled = 0.5
s1.Lpop <- popgen (n = 10000, pSuccess1 = 0.40, pSuccess2 = 0.20, pSample = 0.5)  

head (s1.Lpop[["total population"]])      # Reports Total population
head (s1.Lpop[["sampled population"]])    # Reports Sampled population only
s1.Lpop[["true harvest"]]                 # Reports total harvest



# Now input s1.Lpop to survgen() function to complete "response" column for varying levels of response

s1.Lpop.surv <- survgen (x = s1.Lpop, suc = seq (.2, 1, 0.1))   # Simulate response if population "s1.Lpop" responds to surveys with probabilities from 0.2 : 1 by increments of 0.1.

# Since argument "uns" is left blank it defaults to the same as "suc", making no difference in response rates b/w successful and unsuccessful hunters.

head (s1.Lpop.surv)                                   # Returns a different matrix for each level of response rate.
head (s1.Lpop.surv[ , , "0.5"], 10)                   # Returns first 10 results for response rate of 0.5 only.


for (i in 1:9){                                       # In this case, there are 9 levels of response.
  print (mean(c(s1.Lpop.surv [ , 4, i])))             # Output of this loop should reflect those 9 levels (cont.)
}                                                     # of the sequence put into survgen()


########## Example: Medium pop. w/ successful and unsuccessful hunters not equally likely to report. ###########

# First, simulate a population of 1,000; split into groups of hunters with different rates of harvest (0.35 or 0.25) which are split evenly (for an average harvest rate of 30%) and all have a probability of being sampled = 0.5

s1.Mpop.NRbias <- popgen (n = 1000, pSuccess1 = 0.35, pSuccess2 = 0.20, pSample = 0.5)  

head (s1.Mpop.NRbias[["total population"]])      # Reports Total population (dataframe)
head (s1.Mpop.NRbias[["sampled population"]])    # Reports Sampled population only
s1.Mpop.NRbias[["true harvest"]]                 # Reports total harvest



# Now input s1.Mpop.NRbias to survgen() function to simulate response for varying levels of response

s1.Mpop.NRbias.surv <- survgen (s1.Mpop.NRbias, suc = seq (0.3, 0.9, 0.1), uns = seq (0.1, 0.7, 0.1))   # Simulate response if population "s1.Mpop.NRbias" responds to surveys with different probabilities for successful and unsuccessful hunters

head (s1.Mpop.NRbias.surv)                            # Returns a different matrix for each level of response rate.
head (s1.Mpop.NRbias.surv[ , , "0.5"], 10)            # Returns first 10 results for response rate of 0.5 only. (relative to successful hunters)

for (i in 1:7){                                       # In this case, there are 7 levels of response.
  print (mean(c(s1.Mpop.NRbias.surv [ , 4, i])))      # Output of this loop should reflect somewhere in between given response rates, proportionate to harvest rates and nonresponse bias.
}                                    
