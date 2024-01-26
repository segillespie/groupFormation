library(tidyverse)

numReps <- 15

orgSize <- 20
numGroups <- 5

metricDistro <- 'uniform'
metricParam1 <- 0
metricParam2 <- 120
metricPrecision <- 0 # number of decimal places

df = data.frame(org = rep(1:numReps, each = orgSize))
set.seed(173)
df$metric <- round(runif(orgSize*numReps, min = metricParam1, max = metricParam2), metricPrecision)
df$metricDistro <- metricDistro
df$metricParam1 <- metricParam1
df$metricParam2 <- metricParam2
df$metricPrecision <- metricPrecision



for(org in 1:numReps){
  heuristics(df[df$org == org,] %>% select(org, metric), numGroups, 'random')
}

org =1 
