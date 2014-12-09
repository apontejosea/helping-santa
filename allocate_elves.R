source('helpers.R') # all functions are defined here
source('graphs.R')

########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################

#=======================================================
# Input
#=======================================================
n_elves            <- 50
n_toys             <- n_elves*10
toys_file_name     <- 'toys_rev2.csv'

#=======================================================
# Steps
#=======================================================
system.time(toys     <- read_toys(toys_file_name, nrows=n_toys))
system.time(toys     <- distribute_toys(n_elves, toys))
system.time(schedule <- build_schedule2(toys))
# system.time(schedule <- build_schedule(toys))

# Uncomment to generate graphs
plot_duration(toys)
plot_toy_schedule(toys)
