# This function takes a data frame with a single numeric column called metric
# where metric is the indicator of interest, e.g., GPA and arranges the people
# in the organization into one of several groups.

# The user must provide a number of groups desired (e.g., 3 groups) and it will 
# divide the organization into groups of equal or equal +/- 1 size

# The user must provide the desired heuristic method.  There are currently 7:
#   - top_to_bottom
#   - snake
#   - random
#   - stratified_random
#   - dynamic_least_help
#   - dynamic_most_help
#   - dynamic_alternating_help (to be coded)


heuristics <- function(df, nGroups, heuristicMethod){
  require(dplyr)
  
  availableMethods <- c('top_to_bottom', 'snake', 'random', 'stratified_random', 
                        'dynamic_least_help', 'dynamic_most_help') # 'ordered_grouping', 'dynamic_alternating_help')
  
  if(heuristicMethod == 'allMethods'){
    return(availableMethods)
  }

# Check Input Data --------------------------------------------------------
  # check df.  Should be a data frame with a numeric field titled metric
  if(is.data.frame(df)){ # if df is a dataframe
    if(!('metric' %in% colnames(df))){stop('Input df requires a numeric field named metric.')}
  }else{
    if(mode(df) == 'numeric' & class(df) %in% c('integer', 'numeric', 'double')){
      warning('df supplied as numeric vector.  Converting to a data frame with supplied data as "metric".')
      df = data.frame(metric = df)
    }else{stop('Input df must have a numeric vector')}
  }
  
  # check nGroups
  if(length(nGroups) > 1){stop('nGroups must be single valued')}
  if(class(nGroups) != 'numeric'){stop('nGroups must be numeric')}
  if(nGroups%%1 != 0){
    warning(paste0('Provided nGroups = ', nGroups, '.  Taking the floor. New nGroups is ', floor(nGroups)))
    nGroups <- floor(nGroups)
  }  
  if(nGroups < 1){stop('nGroups must be >= 1')}

  # check heuristicMethod
  if(!(heuristicMethod %in% availableMethods)){
    stop(paste('heuristicMethod must be one of:', paste(availableMethods, collapse = ', ')))
  }
  
  
  
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

# Method 3: Random ----------------------------------------------------------------
  if(heuristicMethod == 'random'){
    # Truly Randomly assigns people to groups 
    df <- df %>% dplyr::arrange(-metric)
    df$groupNumber <- sample(rep(groupDf$groupNumber, ceiling(nPeople/nGroups))[1:nPeople])
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

# Method 5: Dynamic Most Help ---------------------------------------------------
  if(heuristicMethod == 'dynamic_most_help'){ 
    df <- df %>% dplyr::arrange(-metric)
    # Set initial groups 
    df$groupNumber <- c(seq(1:nGroups), nGroups, rep(NA, nPeople - nGroups - 1))
    myIndex <- nGroups + 2 # index for tracking fill
    
    groupDf$curGroupSize <- 1
    groupDf$curGroupSize[groupDf$groupNumber == nGroups] <- 2
    
    # Iterate while not all groups are filled
    while(sum(groupDf$groupSize - groupDf$curGroupSize) > 0){
      
      # initial look, unnecessary for works
      # print(paste(myIndex, 'initial mean'))
      # df %>% 
      #   filter(!is.na(groupNumber)) %>%  # only look at allocated students
      #   group_by(groupNumber) %>% summarise(meanMetric = mean(metric)) %>% 
      #   pivot_wider(names_from = groupNumber, values_from=meanMetric) %>% print()
      
      availableGroups <- groupDf %>% filter(groupSize > curGroupSize) %>% pull(groupNumber)
      # print(paste('Available Groups:', paste(availableGroups, collapse = ', ')))
      
      nextGroupAdd <- df %>% 
        filter(!is.na(groupNumber)) %>%  # only look at allocated students
        group_by(groupNumber) %>% summarise(meanMetric = mean(metric)) %>% # get mean GPAs
        filter(groupNumber %in% availableGroups) %>% #only look at groups that aren't full
        arrange(meanMetric) %>% # find the lowest gpa
        slice(1) %>%  # take the group with the lowest mean gpa
        pull(groupNumber) # get that group number
      
      # print(paste('Next Group Add:', nextGroupAdd))
      
      # Assign next highest student to nextGroupAdd
      df$groupNumber[myIndex] <- nextGroupAdd
      myIndex <- myIndex+1
      groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] <- groupDf[groupDf$groupNumber == nextGroupAdd, 'curGroupSize'] + 1
      
      if(myIndex > nPeople){break}  
    }
    
    df$method <- 'dynamic_most_help'
    
    return(df)
  }

  # Form initial groups, then ask, who needs most help?
  # Arrange in order of decreasing GPA
  # This will always return a deterministic result
  # If 10 in three groups: 1, 2, 3, 3, 3, 2, 2, 1, 1, 1
  

# Method 6: Dynamic Least Help -----------------------------------------
  if(heuristicMethod == 'dynamic_least_help'){
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
    
    df$method <- 'dynamic_least_help'
    
    return(df)
  }
  

# Method 7: Dynamic Alternating Help --------------------------------------
if(heuristicMethod == 'dynamic_alternating_help'){
  stop('steve needs to code this')
  return(df)
}
  
  
# Method 8: Ordered Grouping --------------------------------------
if(heuristicMethod == 'ordered_grouping'){
  # Rack and stack.  This is the worst case scenario for equitable groups.
  stop('steve needs to code this')
  return(df)
}
# if no method found, return nothing
  stop('This should never happen.')
}
