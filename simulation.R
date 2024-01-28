
library(tidyverse)
source('heuristic_methods.R')



numReps <- 100

orgSize <- 20
numGroups <- 5

metricDistro <- 'uniform'
metricParam1 <- 0
metricParam2 <- 120
metricPrecision <- 0 # number of decimal places

df = data.frame(org = rep(1:numReps, each = orgSize), id = rep(1:orgSize, numReps))
set.seed(173)
df$metric <- round(runif(orgSize*numReps, min = metricParam1, max = metricParam2), metricPrecision)
df$metricDistro <- metricDistro
df$metricParam1 <- metricParam1
df$metricParam2 <- metricParam2
df$metricPrecision <- metricPrecision


resultsDf = data.frame()

for(myMethod in heuristics(NA,NA, 'allMethods')){
  for(myOrg in 1:numReps){
    print(paste(myMethod, myOrg))
    resultsDf <- bind_rows(resultsDf, heuristics(df %>% filter(org == myOrg), numGroups, myMethod))
  }
}



resultsDf %>% 
  group_by(org, method, groupNumber) %>% 
  summarize(meanMetric = mean(metric)) %>% 
  ungroup() %>% 
  group_by(org, method) %>% 
  summarize(metricDelta = max(meanMetric) - min(meanMetric)) %>% 
  ggplot(aes(x = metricDelta, color = method)) + 
  geom_density() + 
  theme_minimal()
  #pivot_wider(names_from = method, values_from = metricDelta)

