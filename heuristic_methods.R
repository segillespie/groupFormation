library(tidyverse)



# set.seed(173)
# #df <- data.frame(name = letters[1:10], gpa = rnorm(10, mean = 3, sd = .5))
# df <- data.frame(name = letters[1:10], gpa = runif(10, min = 0, max = 4))
# nGroups <- 3


heuristics <- function(df, nGroups, heuristicMethod){
  # heuristicMethod = {'top_to_bottom', 'snake'}
  
  # data checks and set inputs
  # TBP
  
  
  # Get Initial Data and Create Groups --------------------------------------

  nPeople <- nrow(df)
  groupDf <- data.frame(groupNumber = seq(1:nGroups), groupSize = NA )
  
  # Check Group Size
  if(nPeople%%nGroups == 0){
    groupDf$groupSize <- nPeople/nGroups
  }else{
    groupDf$groupSize <- c(rep(floor(nPeople/nGroups) + 1, nPeople%%nGroups), rep(floor(nPeople/nGroups), (nGroups-nPeople)%%nGroups))
  }
  
  # Method 1: Top to Bottom ----------------------------------------------------------------
  if(heuristicMethod == 'top_to_bottom'){
    # Order by GPA Highest to Lowest
    # Assign Group 1, 2, 3, 1, 2, 3
    # Example, 10 people in 3 groups (A, B, C)
    # A, B, C, A, B, C, A, B, C, A (group of 4 has lowest GPA as additional person)
    df <- df %>% dplyr::arrange(-metric)
    df$groupNumber <- rep(groupDf$groupNumber, ceiling(nPeople/nGroups))[1:nPeople]
    df$method <- 'top_to_bottom'
    return(df)
  }
  # Method 2: Snake -------------------------------------------------------------------
  if(heuristicMethod == 'snake'){
    df <- df %>% dplyr::arrange(-metric)
    df$groupNumber <- rep(c(groupDf$groupNumber, order(groupDf$groupNumber, decreasing = T)), ceiling(ceiling(nPeople/nGroups)/2))[1:nPeople]
    df$method <- 'Snake'
    return(df)
  }

  # Method 3: Rnadom ----------------------------------------------------------------
  if(heuristicMethod == 'random'){
    # Truly Randomly assigns people to groups 
    df <- df[sample(1:nPeople),]
    df$groupNumber <- unlist(mapply(rep, groupDf$groupNumber, groupDf$groupSize))
    df$method <- 'random'
    return(df)
  }
  
  # Method 4: Stratified Random --------------------------------------------------
  if(heuristicMethod == 'stratified_random'){
    numStrata <- max(groupDf$groupSize)
    strata <- rep(seq(1,numStrata, by = 1), each = nGroups)[1:nPeople]
    df <- df %>% dplyr::arrange(-metric)
    df$groupNumber <- data.frame(strata, groupNumber = rep(groupDf$groupNumber, ceiling(nPeople/nGroups))[1:nPeople]) %>% 
      group_by(strata) %>% 
      mutate(groupNumber = sample(groupNumber)) %>% pull(groupNumber)
    df$method <- 'stratified_random'
    return(df)
  }

  # Method 5: Dynamic Top to Bottom ---------------------------------------------------
  if(heuristicMethod == 'dynamic_top_to_bottom'){
    df <- df %>% dplyr::arrange(-metric)
    # Set initial groups 
    df$groupNumber <- c(seq(1:nGroups), nGroups, rep(NA, nPeople - nGroups - 1))
    myIndex <- nGroups + 2 # index for tracking fill
    
    groupDf$curGroupSize <- 1
    groupDf$curGroupSize[groupDf$groupNumber == nGroups] <- 2
    
    # Iterate while not all groups are filled
    while(sum(groupDf$groupSize - groupDf$curGroupSize) > 0){
      
      # initial look, unnecessary for works
      print(paste(myIndex, 'initial mean'))
      df %>% 
        filter(!is.na(groupNumber)) %>%  # only look at allocated students
        group_by(groupNumber) %>% summarise(meanMetric = mean(metric)) %>% 
        pivot_wider(names_from = groupNumber, values_from=meanMetric) %>% print()
      
      availableGroups <- groupDf %>% filter(groupSize > curGroupSize) %>% pull(groupNumber)
      print(paste('Available Groups:', paste(availableGroups, collapse = ', ')))
      
      nextGroupAdd <- df %>% 
        filter(!is.na(groupNumber)) %>%  # only look at allocated students
        group_by(groupNumber) %>% summarise(meanMetric = mean(metric)) %>% # get mean GPAs
        filter(groupNumber %in% availableGroups) %>% #only look at groups that aren't full
        arrange(meanMetric) %>% # find the lowest gpa
        slice(1) %>%  # take the group with the lowest mean gpa
        pull(groupNumber) # get that group number
      
      print(paste('Next Group Add:', nextGroupAdd))
      
      # Assign next highest student to nextGroupAdd
      df$groupNumber[myIndex] <- nextGroupAdd
      myIndex <- myIndex+1
      groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] <- groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] + 1
      
      if(myIndex > nPeople){break}  
    }
    
    df$method <- 'Dynamic Top to Bottom'
    
    return(df)
  }

  # Form initial groups, then ask, who needs most help?
  # Arrange in order of decreasing GPA
  # This will always return a deterministic result
  # If 10 in three groups: 1, 2, 3, 3, 3, 2, 2, 1, 1, 1
  if(heuristicMethod == 'dynamic_bottom_to_top'){
    df <- df %>% dplyr::arrange(-metric)
    # Set initial groups 
    df$groupNumber <- c(seq(1:nGroups), rep(NA, nPeople - nGroups))
    myIndex <- nPeople # index for tracking fill
    
    groupDf$curGroupSize <- 1
    
    # Iterate while not all groups are filled
    while(sum(groupDf$groupSize - groupDf$curGroupSize) > 0){
      
      # initial look, unnecessary for works
      availableGroups <- groupDf %>% filter(groupSize > curGroupSize) %>% pull(groupNumber)
      #print(paste('Available Groups:', paste(availableGroups, collapse = ', ')))
      
      nextGroupAdd <- df %>% 
        filter(!is.na(groupNumber)) %>%  # only look at allocated students
        group_by(groupNumber) %>% summarise(meanMetric = mean(metric)) %>% # get mean metrics
        filter(groupNumber %in% availableGroups) %>% #only look at groups that aren't full
        arrange(-meanMetric) %>% # find the highest gpa
        slice(1) %>%  # take the group with the highest mean gpa
        pull(groupNumber) # get that group number
      
      #print(paste('Next Group Add:', nextGroupAdd))
      
      # Assign next highest student to nextGroupAdd
      df$groupNumber[myIndex] <- nextGroupAdd
      myIndex <- myIndex-1
      groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] <- groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] + 1
      
      if(myIndex > nPeople){break}  
    }
    
    df$method <- 'Dynamic Bottom to Top'
    
    return(df)
  }
  
  # if no method found, return nothing
  return()
}
