#---compare sims to empirical 
library(data.table)
library(ggplot2)
library(ggpubr)
library(combinat)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#sim results
#sim_df <- read.csv('./sim_output/heuristic_sims_combined.csv') this is the one with resampling on the randoms
sim_df <- read.csv('./sim_output/heuristic_all_sims_v2.csv') #this does not resample the randoms


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

emp_df$students_per_group <- emp_df$class_size/emp_df$num_groups



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
sim_df$students_per_group <- sim_df$class_size/sim_df$num_groups
sim_df$even_groups <- sim_df$students_per_group%%1==0

sim_df$method[sim_df$method == 'linear_draft'] <- 'Linear'
sim_df$method[sim_df$method == 'snake_draft'] <- 'Snake'
sim_df$method[sim_df$method == 'tax_the_rich'] <- 'Tax the Rich'
sim_df$method[sim_df$method == 'feed_the_poor'] <- 'Feed the Poor'
sim_df$method[sim_df$method == 'alternating_convergence'] <- 'Alternating Convergence'
sim_df$method[sim_df$method == 'random'] <- 'Random'
sim_df$method[sim_df$method == 'stratified_random'] <- 'Stratified Random'


plot_df <- sim_df[, .(avg_error = mean(dist_from_best), sd_error = sd(dist_from_best)), by = c('method', 'students_per_group')]

unique(plot_df$method)

# plot_df$method[plot_df$method == 'linear_draft'] <- 'Linear'
# plot_df$method[plot_df$method == 'snake_draft'] <- 'Snake'
# plot_df$method[plot_df$method == 'tax_the_rich'] <- 'Tax the Rich'
# plot_df$method[plot_df$method == 'feed_the_poor'] <- 'Feed the Poor'
# plot_df$method[plot_df$method == 'alternating_convergence'] <- 'Alternating Convergence'
# plot_df$method[plot_df$method == 'random'] <- 'Random'
# plot_df$method[plot_df$method == 'stratified_random'] <- 'Stratified Random'

#scale y axis from 0 to 1
#change legend title to Method
p1 <- ggplot(plot_df, aes(x = students_per_group, y = avg_error, color = factor(method, levels = (c('Alternating Convergence', 'Tax the Rich', 'Snake', 'Stratified Random','Linear','Random','Feed the Poor'))))) + geom_point() + geom_line() + theme_bw(base_size = 16) + 
  # labs(title = 'Mean Error (Compared to Optimal Solution) for Heuristic Methods\n by Class Size and Method for 1,000 Simulated Classes', x = 'Class Size', y = 'Mean Error') + 
  labs(title = '', x = '# Students', y = 'Mean Error Compared\nto MILP Solution') + 
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_blank(), legend.box.background = element_rect(colour = "black"), legend.box = 'horizontal', legend.position = 'bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(0, 1.05)) + scale_color_brewer(name = '', palette = 'Set1')

#ggsave('~/Downloads/error_by_method.jpg', width = 9, height = 6, dpi = 600)

plot_df <- sim_df[, .(avg_error = mean(dist_from_best), sd_error = sd(dist_from_best)), by = c('method', 'class_size')]


p2 <- ggplot(plot_df, aes(x = class_size, y = avg_error, color = factor(method, levels = (c('Alternating Convergence', 'Tax the Rich', 'Snake', 'Stratified Random','Linear','Random','Feed the Poor'))))) + geom_point() + geom_line() + theme_bw(base_size = 16) + 
  # labs(title = 'Mean Error (Compared to Optimal Solution) for Heuristic Methods\n by Class Size and Method for 1,000 Simulated Classes', x = 'Class Size', y = 'Mean Error') + 
  labs(title = '', x = 'Class Size', y = '') + 
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_blank(), legend.box.background = element_rect(colour = "black"), legend.box = 'horizontal', legend.position = 'bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(0, 1.05)) + scale_color_brewer(name = '', palette = 'Set1')

ggarrange(p1, p2, common.legend = TRUE, legend = "bottom")

ggsave('~/Downloads/error_combined.jpg', width = 10, height = 4, dpi = 600)






sim_plot_df <- sim_df
sim_plot_df$even_groups[sim_plot_df$even_groups == FALSE] <- 'Uneven'
sim_plot_df$even_groups[sim_plot_df$even_groups == TRUE] <- 'Even'

ggplot(sim_plot_df, aes(x = dist_from_best, y = even_groups, color = even_groups)) + geom_boxplot() + theme_bw() + scale_color_brewer(name = '', palette = 'Set1') +
  facet_wrap(~factor(method, levels = (c('Alternating Convergence', 'Tax the Rich', 'Snake', 'Stratified Random','Linear','Random','Feed the Poor'))), ncol = 4) + theme(strip.background =element_rect(fill=NA), strip.text = element_text(face="bold")) +
  xlab('Error Compared to MILP Solution') + ylab('') + theme(legend.position = 'none') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave('~/Downloads/uneven_performance.jpg', width = 10, height = 4, dpi = 600)



ggplot(sim_df, aes(x = dist_from_best, y = factor(method, levels = rev(c('Alternating Convergence', 'Tax the Rich', 'Snake', 'Stratified Random','Linear','Random','Feed the Poor'))))) + 
  geom_boxplot() + theme_bw(base_size = 16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xlab('Error Compared to MILP Solution') + ylab('Method')
ggsave('~/Downloads/overall_performance.jpg', width = 10, height = 4, dpi = 600)


?theme

ggarrange(
  bxp, dp, labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)


# plot_df2 <- sim_df
# #label if students_per_group is a whole number
# plot_df2$even_groups <- plot_df2$students_per_group%%1==0
# #plot_df2 <- plot_df2[, .(avg_error = mean(dist_from_best), sd_error = sd(dist_from_best)), by = c('method', 'even_groups')]
# 
# ggplot(plot_df2, aes(x = dist_from_best, y = even_groups, color = method)) + geom_boxplot() + theme_minimal()


test <- sim_df[method == 'Alternating Convergence']

t1 <- test$dist_from_best[test$even_groups == TRUE]
t2 <- test$dist_from_best[test$even_groups == FALSE]

t.test(t1, t2, alternative = 'less')



mod <- lm(dist_from_best ~ students_per_group + class_size + even_groups, data = test)
summary(mod)



#---------make matrix comparison vis

sim_df <- read.csv('./sim_output/heuristic_sims_combined.csv')

methods <- unique(sim_df$method)
out_mat <- matrix(NA, nrow = length(methods), ncol = length(methods))
colnames(out_mat) <- methods
row.names(out_mat) <- methods

combinations <- data.frame(t(combn(methods, m = 2)))
combinations <- data.frame(expand.grid(methods, methods))

colnames(combinations) <- c('method_1','method_2')

out <- list()
for(i in 1:nrow(combinations)){
  m1 <- combinations[i, 'method_1']
  m2 <- combinations[i, 'method_2']
  
  comp <- sim_df[sim_df$method == m1,]$delta - sim_df[sim_df$method == m2,]$delta
  m1_win <- length(comp[comp < 0]) / length(comp)
  out[[i]] <- m1_win
}
combinations$win_rate <- unlist(out)
combinations$method_1 <- factor(combinations$method_1, levels = rev(methods))
combinations$method_2 <- factor(combinations$method_2, levels = methods)
combinations$win_rate[combinations$method_1 == combinations$method_2] <- NA
combinations$win_percent <- paste0(round(combinations$win_rate * 100, 2), '%')

ggplot(combinations, aes(x = method_2, y = method_1)) + geom_tile(aes(fill = win_rate * 100)) + theme_minimal(base_size = 12) +
  geom_text(aes(label = win_percent), size = 3) + scale_fill_gradient(low = 'white', high = 'red') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab('Method 2') + ylab('Method 1') +
  guides(fill = guide_legend(title = 'Win Percentage (%)'))  +
  ggtitle('% of Runs where Method 1 Beats Method 2') + theme(plot.title = element_text(hjust = 0.5))
  
ggsave('~/Downloads/method_comparison.jpg', width = 10, height = 6, dpi = 600)
            