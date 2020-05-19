# Hunter Simulation
#
#
#
#
########## Population generator function ######################################

# The below function simulates a population and returns a list with 3 elements. 1st element is a dataframe containing the entire population. 2nd element is a matrix containing sampled hunters only, and the 3rd is the number of total harvests by the simulated population. Response is not calculated here.

# ARGUMENTS:
# n = pop size
# pSuccess1 = prob. of hunter group 1 harvesting
# pSuccess2 = prob. of hunter group 2 harvesting. Is equal to pSuccess1 by default (for scenarios where all hunters are equally likely to harvest)
# pSample is probability a hunter is sampled.

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

# This will return an array with each consecutive third dimension being a simulation of survey response by increasing levels based on the input for argument "suc". 

# ARGUMENTS: 
# "x" = a list, from popgen() output
# "suc" = a seq() with length >2 and <=9 that dictates response rates of successful hunters
# "uns" = same as suc, but seq() of response rates for unsuccessful hunters.

survgen <- function (x, suc, uns = suc){ 
  
  
  # First some quick checks to make sure sequences were inputted correctly:
  if (length (suc) > 9 || length (suc) <= 2 || length (uns) > 9 || length (uns) <= 2){
    stop("length of responding rate sequences must be > 2 and <= 9")
  }
  if (length (suc) != length (uns)){
    stop ("length of 'suc' & 'uns' must be of equal length")
  }
  
  
# Now to create the array containing data of sampled hunters from pre-simulated pop, and replicate by as many terms as in given sequence
  
  samp <- array(x[[2]], dim = c(nrow(x[[2]]), ncol(x[[2]]), length(suc)))  
  
  colnames(samp) <- c("Group", "Harvest", "Sampled", "Response")
  dimnames(samp)[[3]] <- c(as.character(suc))    # Adding reporting %'s for easier recall later
  
  
# Now to fill in the array:
  
  for (j in 1:nrow(samp)){
    
    samp [j , 4, 1] <- rbinom (1, 1, suc[1])    # For first layer of array apply response based off prob. suc[1] (prob. that a successful hunter responds, level 1)
    samp [j , 4, 2] <- rbinom (1, 1, suc[2])    # For second layer apply response of prob. suc[2]..... etc.
    if (length(suc) > 2){                       # Will only execute if length of given "suc" is > 2
      samp [j , 4, 3] <- rbinom (1, 1, suc[3])
    }
    if (length(suc) > 3){
      samp [j , 4, 4] <- rbinom (1, 1, suc[4])
    }
    if (length(suc) > 4){
      samp [j , 4, 5] <- rbinom (1, 1, suc[5])
    }
    if (length(suc) > 5){
      samp [j , 4, 6] <- rbinom (1, 1, suc[6])
    }
    if (length(suc) > 6){
      samp [j , 4, 7] <- rbinom (1, 1, suc[7])
    }
    if (length(suc) > 7){
      samp [j , 4, 8] <- rbinom (1, 1, suc[8])
    }
    if (length(suc) > 8){
      samp [j , 4, 9] <- rbinom (1, 1, suc[9])
    }}
  
  
  return(samp)
}
########## SCENARIO 1: Simple Random Sample by phone w/ follow-up ###########################

########## Example of functions: Large pop. w/ successful and unsuccessful hunters equally likely to report ##########

# Simulate a population of 10,000; split into groups of hunters with different rates of harvest (0.4 or 0.2) which are split evenly (for an average harvest rate of 30%) and all have a probability of being sampled = 0.5
s1.Lpop <- popgen (n = 10000, pSuccess1 = 0.40, pSuccess2 = 0.20, pSample = 0.5)  

head (s1.Lpop[["total population"]])      # Reports Total population
head (s1.Lpop[["sampled population"]])    # Reports Sampled population only
s1.Lpop[["true harvest"]]                 # Reports total harvest



# Now input s1.Lpop to survgen() function to complete "response" row for varying levels of response

s1.Lpop.surv <- survgen (s1.Lpop, seq (.2, 1, 0.1))   # Simulate response if population "s1.Lpop" responds to surveys with probabilities from 0.2 : 1 by increments of 0.1.

head (s1.Lpop.surv)                                   # Returns a different matrix for each level of response rate.
head (s1.Lpop.surv[ , , "0.5"], 10)                   # Returns first 10 results for response rate of 0.5 only.


for (i in 1:9){                                       # In this case, there are 9 levels of response.
  print (mean(c(s1.Lpop.surv [ , 4, i])))             # Output of this loop should reflect those 9 levels (cont.)
}                                                     # of the sequence put into survgen()

