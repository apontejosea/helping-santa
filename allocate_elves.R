source('helpers.R') # all functions are defined here

##########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
##########################################################

#=======================================================
# Input
#=======================================================
n_elves            <- 5
toys_file_name     <- 'toys_sample.csv'
solution_file_name <- 'initial_solution.yml'

#=======================================================
# Steps 
#=======================================================
toys     <- read_toys(toys_file_name)
s0       <- read_solution(solution_file_name)
build_schedule(s0, toys)

# plot_duration(toys)
# plot_toy_schedule(toys)
