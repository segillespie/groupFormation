library(tidyverse)
source('heuristic_methods.R')

numReps <- 1

orgSize <- 10
numGroups <- 3

metricDistro <- 'uniform'
metricParam1 <- 1
metricParam2 <- 4
metricPrecision <- 1 # number of decimal places

df = data.frame(org = rep(1:numReps, each = orgSize), id = rep(1:orgSize, numReps))
set.seed(173)
df$metric <- round(runif(orgSize*numReps, min = metricParam1, max = metricParam2), metricPrecision)
df$metricDistro <- metricDistro
df$metricParam1 <- metricParam1
df$metricParam2 <- metricParam2
df$metricPrecision <- metricPrecision

df = df %>% select(org, id, metric)
df$rank = rank(-df$metric, ties.method = 'random')


resultsDf = data.frame()

for(myMethod in heuristics(NA,NA, 'allMethods')){
  for(myOrg in 1:numReps){
    print(paste(myMethod, myOrg))
    resultsDf <- bind_rows(resultsDf, heuristics(df %>% filter(org == myOrg), numGroups, myMethod))
  }
}


resultsDf %>% 
  mutate(groupNumber = LETTERS[groupNumber]) %>% 
  pivot_wider(id_cols = c(org, id, metric, rank), names_from = method, values_from = groupNumber) %>% 
  select(-org) %>% write.csv('heuristicExample.csv',row.names = F)

