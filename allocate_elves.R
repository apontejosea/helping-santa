source('helpers.R') # all functions are defined here

########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################

#=======================================================
# Input
#=======================================================
n_elves            <- 5
toys_file_name     <- 'toys_sample.csv'

#=======================================================
# Steps 
#=======================================================
system.time(toys     <- read_toys(toys_file_name))
system.time(toys     <- distribute_toys(5, toys))
system.time(toys     <- build_schedule(toys))

# Uncomment to generate graphs
# plot_duration(toys)
# plot_toy_schedule(toys)
