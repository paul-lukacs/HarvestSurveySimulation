  #
  # Harvest Survey Simulation demonstration
  #

  library(survey)
  library(dplyr)

  ######## set up the simulation parameters ###########
  
  nHunters <- 1000   # number of hunters
  pSuccess <- 0.4    # probability of successful harvest
  pSample  <- 0.6    # probability that a hunter is selected to receive a survey
  pRespond <- 0.8    # probability that a hunter responds to the survey
  
  harvestData <- matrix( NA, nrow=nHunters, ncol=3 ) # matrix to hold harvest survey data
  colnames(harvestData) <- c( "harvest", "sampled", "respond" )
  
  
  ######### simulate harvest ###########################
  
  for( i in 1:nHunters ){
    harvestData[i,1] <- rbinom( 1, 1, pSuccess ) # did the hunter harvest (0/1)
    harvestData[i,2] <- rbinom( 1, 1, pSample )  # was the hunter selected for a survey (0/1)
    harvestData[i,3] <- rbinom( 1, 1, pRespond ) # did the hunter respond to the survey (0/1)
  }
  harvestData <- as.data.frame( harvestData )
  
  ######### analyze the data ############################
  
  trueHarvest <- sum( harvestData$harvest ) # sum of success is the number harvested
  
  # harvest estimate with complete sample data
  tmp <- filter( harvestData, sampled == 1 ) # filter down to the sampled hunters
  
  # create the survey design object (this uses the survey package)
  survDataComplete <- svydesign( ~1,                                 # sample ID's (we don't have any yet)
                                probs = nrow(tmp)/nrow(harvestData), # sampling probability
                                data = tmp )                         # data set
  
  completeSample <- svytotal( ~harvest, survDataComplete ) # run the analysis
  
  # harvest estimate with missing data from non-response
  tmp <- filter( harvestData, respond == 1 ) # filter down to the responding hunters
  
  # create the survey design object (this uses the survey package)
  survDataNonresponse <- svydesign( ~1,                                 # sample ID's (we don't have any yet)
                                 probs = nrow(tmp)/nrow(harvestData), # sampling probability
                                 data = tmp )                         # data set
  
  nonResponseSample <- svytotal( ~harvest, survDataNonresponse ) # run the analysis
  