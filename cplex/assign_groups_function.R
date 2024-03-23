# AUTHOR: Matthew Dabkowski 
# ORGANIZATION: USMA's DSE
# DATE: 19 JAN 2024

# PURPOSE: This script builds a given number of project groups for a section such that...
# 1. Group sizes are as equal as possible, 
# 2. The range of the groups' mean CQPAs is minimized, and
# 3. Students pre-designated to be paired together are placed in the same group(s).

# INPUT: The desired number of groups and a .csv file with student names, CQPAs, and pre-designated pairings (if any).
# OUTPUT: A list of optimal groups with its (globally minimal) CQPA range.

# APPROACH: Mixed-integer linear programming
# See https://www.r-orms.org/mixed-integer-linear-programming/packages/modelling-milp/

#---kloo mods: make make into a function to support looping through easier

#no easy way to set time limit for cplex through ROI (that i could find) - so we're setting it with this util function
#see end for argument about max time and what to do if you hit it
library(R.utils)
withTimeout({

suppressWarnings(suppressMessages({
  library(magrittr)
  library(ompr)
  library(ROI)
  #library(ROI.plugin.glpk) # connects the ROI package to GLPK
  library(ompr.roi)
  library(ROI.plugin.cplex) # connects the ROI package to CPLEX
  library(dplyr)
  library(evaluate)

options(scipen=999)


#---arguments from command line
args <- commandArgs(trailingOnly = TRUE)

student_data <- read.csv(args[1]) # read in the student data
num_groups <- as.numeric(args[2])


run_cplex_assignment <- function(student_data, num_groups){
  m <- num_groups
  
  CQPA <- student_data[,2] # store vector of student CQPAs
  n <- length(CQPA) # calculate and store the number of students
  
  # build a list of students that must be in the same group(s) 
  forced_group_IDs <- unique(student_data$group[which(student_data$group != 0)])
  forced_groups <- vector(mode='list', length=length(forced_group_IDs))
  index <- 1
  for (i in forced_group_IDs) {
    forced_groups[[index]] <- which(student_data$group == forced_group_IDs[index])
    index <- index + 1
  }
  
  # calculate and store the vector of group sizes (s)
  MOD <- n %% m
  QUOTIENT <- n %/% m
  # If MOD = 0, create num_groups of size QUOTIENT,
  # Else, create MOD groups of size QUOTIENT + 1, and num_groups - MOD of size QUOTIENT 
  if (MOD == 0) {
    s <- rep(QUOTIENT, m)
  } else {
    s <- append(rep(QUOTIENT + 1, MOD), rep(QUOTIENT, m - MOD))
  }
  
  # build the mixed integer program
  #max_time <- 60 # in minutes
  B <- 1000 # a suitably "big" number
  if (index == 1) {
    result <-  
        MIPModel() %>% 
        add_variable(y, type = "continuous") %>% 
        add_variable(z, type = "continuous") %>%
        add_variable(x[i, j], type = "binary", i = 1:n, j = 1:m) %>%
        add_variable(b[j], type = "binary", j = 1:m) %>%
        set_objective(y-z, sense="min") %>%
        add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>%
        add_constraint(sum_expr(x[i, j], i = 1:n) == s[j], j = 1:m) %>%
        add_constraint((1 / s[j]) * sum_expr((CQPA[i] * x[i, j]), i = 1:n) <= y, j = 1:m) %>%
        add_constraint((1 / s[j]) * sum_expr(CQPA[i] * x[i, j], i = 1:n) >= z, j = 1:m) %>%
        add_constraint((1 / s[j]) * sum_expr(CQPA[i] * x[i, j], i = 1:n) - B * b[j] <= z, j = 1:m) %>%
        add_constraint(sum_expr(b[j], j = 1:m) <= m - 1) %>%
        # solve the MILP
        solve_model(with_ROI("cplex", verbose = TRUE))
    #solve_model(with_ROI("glpk", verbose = TRUE, tm_lim = 5))
  } else {
    result <- MIPModel() %>% 
      add_variable(y, type = "continuous") %>% 
      add_variable(z, type = "continuous") %>%
      add_variable(x[i, j], type = "binary", i = 1:n, j = 1:m) %>%
      add_variable(b[j], type = "binary", j = 1:m) %>%
      set_objective(y-z, sense="min") %>%
      add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>%
      add_constraint(sum_expr(x[i, j], i = 1:n) == s[j], j = 1:m) %>%
      add_constraint((1 / s[j]) * sum_expr((CQPA[i] * x[i, j]), i = 1:n) <= y, j = 1:m) %>%
      add_constraint((1 / s[j]) * sum_expr(CQPA[i] * x[i, j], i = 1:n) >= z, j = 1:m) %>%
      add_constraint((1 / s[j]) * sum_expr(CQPA[i] * x[i, j], i = 1:n) - B * b[j] <= z, j = 1:m) %>%
      add_constraint(sum_expr(b[j], j = 1:m) <= m - 1) %>%
      # add constraints to force group assignments
      add_variable(w[k, j], type = "binary", j = 1:m, k = 1:(index - 1)) %>%
      add_constraint(sum_expr(w[k, j], j = 1:m) == 1, k = 1:(index - 1)) %>%
      add_constraint(w[k, j] <= x[l, j], j = 1:m, k = 1:(index - 1), l = 1:n, l %in% forced_groups[[k]]) %>%
      add_constraint(w[k, j] >= sum_expr(x[l, j], l = 1:n, l %in% forced_groups[[k]]) - length(forced_groups[[k]]) - 1, j = 1:m, k = 1:(index - 1)) %>%
      # solve the MILP
      #solve_model(with_ROI("glpk", verbose = TRUE, tm_lim = 5))
      solve_model(with_ROI("cplex", verbose = TRUE)) 
  }
  
  print(result)
}

run_cplex_assignment(student_data, num_groups)
# withTimeout({
#   run_cplex_assignment(student_data, num_groups)
# },  timeout = .5, onTimeout = "silent")

}))


}, timeout = 60, onTimeout = 'silent')

