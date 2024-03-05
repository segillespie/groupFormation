library(tidyverse)
source('heuristic_methods.R')

myFile <- 'student_data_5.csv'
df <- read.csv(myFile)
df <- df %>% 
  mutate(org = 1, id = last_name, metric = CQPA, metricDistro = 'student_data_5.csv') %>% 
  select(org, id, metric, metricDistro)

numGroups <- 5
numReps <- 1

resultsDf = data.frame()

for(myMethod in heuristics(NA,NA, 'allMethods')){
  for(myOrg in 1:numReps){
    print(paste(myMethod, myOrg))
    resultsDf <- bind_rows(resultsDf, heuristics(df %>% filter(org == myOrg), numGroups, myMethod))
  }
}

resultsDf %>% 
#  mutate(groupNumber = LETTERS[groupNumber]) %>% 
  pivot_wider(id_cols = c(org, id, metric), names_from = method, values_from = groupNumber) %>% 
  select(-org) %>% 
  write.csv(paste0('heuristic_', myFile),row.names = F)





