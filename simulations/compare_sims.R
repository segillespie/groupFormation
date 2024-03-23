#---compare sims to empirical 
library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#sim results
sim_df <- read.csv('./sim_output/heuristic_sims_combined.csv')


#get empirical results in one table
emp_files <- list.files('./cplex_output', full.names = TRUE)

all <- list()
counter <- 1
for(f in emp_files){
  tmp <- read.csv(f)
  all[[counter]] <- tmp
  counter <- counter + 1
}

emp_df <- rbindlist(all)
rm(all)
rm(counter)
rm(tmp)

sim_df$dist_from_best <- NA
pb <- txtProgressBar(min = 0, max = nrow(sim_df), style = 3)
for(i in 1:nrow(sim_df)){
  row <- sim_df[i,]
  
  best <- emp_df[emp_df$sample == row$sample & emp_df$class_size == row$class_size & emp_df$num_groups == row$num_groups,]$obj_value
  dist_from_best <- row$delta - best  
  sim_df[i, 'dist_from_best'] <- dist_from_best
  setTxtProgressBar(pb, i)
}

#average error for each method
sim_df <- data.table(sim_df)

plot_df <- sim_df[, .(avg_error = mean(dist_from_best)), by = c('method', 'class_size')]

#scale y axis from 0 to 1
ggplot(plot_df, aes(x = class_size, y = avg_error, color = method)) + geom_point() + geom_line() + theme_minimal(base_size = 16) + 
  labs(title = 'Average Error by Class Size and Method', x = 'Class Size', y = 'Average Error') + theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(limits = c(0, 1)) + scale_color_brewer(palette = 'Set1')

ggsave('./sim_output/error_by_method.png', width = 10, height = 6, dpi = 600)



