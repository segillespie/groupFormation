#---run heuristic sims for paper
library(data.table)

#load in methods
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../heuristic_methods.R")

#takes dataframe, num_groups, method
#available methods: 
#   - linear_draft
#   - snake_draft
#   - random
#   - stratified_random
#   - tax_the_rich
#   - feed_the_poor
#   - alternating_convergence 

#dataframe is 1-column, column is named metric (e.g., should be GPAs with length = number of people in class)

#data files
class_files <- list.files('../input_data/', full.names = TRUE)
class_files <- class_files[grep('class_size', class_files)]

methods_deterministic <- c('linear_draft', 'snake_draft', 'tax_the_rich', 'feed_the_poor', 'alternating_convergence')
methods_random <- c('random', 'stratified_random')

#run deterministic methods --> fast enough to run serial 
big_out <- list()
out_count <- 1
for(m in methods_deterministic){
  for(cl in class_files){
    #read in data
    class_samples <- read.csv(cl, stringsAsFactors = FALSE)
    num_groups <- 2:floor(ncol(class_samples)/3) #start with 2 groups, go until num groups that would have at least 3 people
    
    print(paste0('running for class file', ' ', cl, ' with method', ' ', m))
    pb <- txtProgressBar(min = 0, max = (length(num_groups) * nrow(class_samples)), style = 3)
    counter <- 0
    
    for(ng in num_groups){
      for(sample in 1:nrow(class_samples)){
        df_to_run <- data.frame(metric = as.vector(t(class_samples[sample,])))
        
        assignments <- data.table(heuristics(df_to_run, as.numeric(ng), m))
        
        #group_averages
        tmp <- assignments[, .(avg = mean(metric)), by = .(group = groupNumber)]
        delta <- max(tmp$avg) - min(tmp$avg)

        tmp_df <- data.frame(method = m, sample = sample, class_size = ncol(class_samples), num_groups = ng, delta = delta)
        
        big_out[[out_count]] <- tmp_df
        setTxtProgressBar(pb, counter)
        counter <- counter + 1
        out_count <- out_count + 1
      }
    }
  }
}

determ_methods <- rbindlist(big_out)
#write.csv(determ_methods, './sim_output/heuristic_sims_deterministic.csv', row.names = FALSE)

#run the random methods serial --> wicked slow
# big_out <- list()
# out_count <- 1
# for(m in methods_random){
#   for(cl in class_files){
#     #read in data
#     class_samples <- read.csv(cl, stringsAsFactors = FALSE)
#     num_groups <- 2:floor(ncol(class_samples)/3) #start with 2 groups, go until num groups that would have at least 3 people
#     
#     print(paste0('running for class file', ' ', cl, ' with method', ' ', m))
#     pb <- txtProgressBar(min = 0, max = (length(num_groups) * nrow(class_samples)), style = 3)
#     counter <- 0
#     
#     for(ng in num_groups){
#       for(sample in 1:nrow(class_samples)){
#         df_to_run <- data.frame(metric = as.vector(t(class_samples[sample,])))
#         
#         #this is to deal with randomness
#         delta_running <- list()
#         for(i in 1:500){
#           assignments <- data.table(heuristics(df_to_run, as.numeric(ng), 'linear_draft'))
#           
#           #group_averages
#           tmp <- assignments[, .(avg = mean(metric)), by = .(group = groupNumber)]
#           #diffs <- diff(tmp$avg)
#           delta <- max(tmp$avg) - min(tmp$avg)
#           delta_running[[i]] <- delta
#         }
#         
#         delta_final <- mean(unlist(delta_running))
#         
#         tmp_df <- data.frame(method = m, sample = sample, class_size = ncol(class_samples), num_groups = ng, delta = delta_final)
#         
#         big_out[[out_count]] <- tmp_df
#         setTxtProgressBar(pb, counter)
#         counter <- counter + 1
#         out_count <- out_count + 1
#       }
#     }
#   }
# }
# 
# rand_methods <- rbindlist(big_out)

#parallelized random methods --> parallelizing across samples
library(doSNOW)
library(foreach)
library(doRNG)

cl <- makeCluster(10)

#everything you want to execute on each cluster
clusterEvalQ(cl,{
  source("../heuristic_methods.R")
  library(data.table)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
})

registerDoSNOW(cl)

#internal loop that runs for each sample, returns average delta
run_iters <- function(data, ng, m){
  df_to_run <- data.frame(metric = data)
  
  delta_running <- list()
  for(i in 1:500){
    
    assignments <- data.table(heuristics(df_to_run, as.numeric(ng), m))
    
    #group_averages
    tmp <- assignments[, .(avg = mean(metric)), by = .(group = groupNumber)]
    #diffs <- diff(tmp$avg)
    delta <- max(tmp$avg) - min(tmp$avg)
    delta_running[[i]] <- delta
  }
  
  delta_final <- mean(unlist(delta_running))
  return(delta_final)
}

big_out <- list()
out_count <- 1
for(m in methods_random){
  for(cf in class_files){
    #read in data
    class_samples <- read.csv(cf, stringsAsFactors = FALSE)
    num_groups <- 2:floor(ncol(class_samples)/3) #start with 2 groups, go until num groups that would have at least 3 people

    for(ng in num_groups){
      print(paste0('running for class file', ' ', cf, ' with method', ' ', m, ' and num_groups', ' ', ng))
      
      iterations <- nrow(class_samples)
      pb <- txtProgressBar(max = iterations, style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      #%dopar% is the standard method, but there are weird things with random seeds that the %dorng% library fixed
      results <- foreach(i = 1:iterations, .options.snow = opts) %dorng%{
        run_iters(as.vector(t(class_samples[i,])), as.numeric(ng), m)
      }
      
      delta_final <- unlist(results)
      
      tmp_df <- data.frame(method = m, sample = 1:nrow(class_samples), class_size = ncol(class_samples), num_groups = ng, delta = delta_final)
      big_out[[out_count]] <- tmp_df
      out_count <- out_count + 1
    }
  }
}

stopCluster(cl)

random_methods <- rbindlist(big_out)
# write.csv(random_methods, './sim_output/heuristic_sims_random.csv', row.names = FALSE)

combined <- rbind(determ_methods, random_methods)
write.csv(combined, './sim_output/heuristic_sims_combined.csv', row.names = FALSE)


