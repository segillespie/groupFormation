#---can't figure out how to capture the verbose output from CPLEX unless I create a new environment using system() and capture everything that comes out
library(data.table)

start_time <- Sys.time()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

class_samples <- read.csv('../input_data/class_size_10', stringsAsFactors = FALSE)
num_groups <- 2:floor(ncol(class_samples)/3) #start with 2 groups, go until num groups that would have at least 3 people

df_out <- list()
counter <- 1
pb <- txtProgressBar(min = 0, max = length(num_groups)*nrow(class_samples), style = 3)
for(ng in num_groups){
  for(i in 1:nrow(class_samples)){
    gpa <- class_samples[i,]
    names(gpa) <- NULL
    gpa <- unlist(as.vector(gpa))
    student_data <- data.frame(name = paste0('Student_', 1:length(class_samples[i,])), gpa = gpa)
    
    write.csv(student_data, file = 'tmp.csv', row.names = FALSE)
    
    run_string <- paste0('Rscript assign_groups_function.R tmp.csv ', ng)
    

    out <- system(run_string, intern = TRUE, ignore.stderr = TRUE)
    
    
    total_ticks <- as.numeric(gsub('.*\\((.*) ticks\\)', '\\1', tail(out[grep('Total ', out)], n = 1)))
    total_seconds <- as.numeric(gsub('.* (.*) sec.*', '\\1', tail(out[grep('Total ', out)], n = 1)))
    obj_value <- as.numeric(gsub('.*: (.*)', '\\1', tail(out[grep('Objective value', out)], n = 1)))
    sim_status <- tail(out[grep('Status: ', out)], n = 1)
    
    class_size <- ncol(class_samples)
    
    if(length(obj_value) == 0){
      obj_value <- NA
      sim_status <- 'failed'
      df_out[[counter]] <- data.frame(sample = i, class_size, num_groups = ng, total_ticks, total_seconds, obj_value, sim_status)
    } else{
      df_out[[counter]] <- data.frame(sample = i, class_size, num_groups = ng, total_ticks, total_seconds, obj_value, sim_status)
    }

    counter <- counter + 1
    setTxtProgressBar(pb, counter)
  }
  print(paste0('finished runs for group size: ', ng))
}

  
final <- rbindlist(df_out)
write.csv(final, file = '../simulations/cplex_output/CPLEX_results_class_size_10.csv', row.names = FALSE)

end_time <- Sys.time()

end_time - start_time
