source('helpers.R') # all functions are defined here
source('graphs.R')

########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################

#=======================================================
# Input
#=======================================================
n_elves            <- 6
n_toys             <- n_elves*200
toys_file_name     <- 'toys_rev2.csv'

#=======================================================
# Steps
#=======================================================
system.time(toys     <- read_toys(toys_file_name, nrows=n_toys))
system.time(toys     <- distribute_toys(n_elves, toys))
system.time(schedule <- build_schedule2(toys))


# Uncomment to generate graphs
plot_duration(schedule)
plot_toy_schedule(schedule)
plot_elf_schedule(schedule)
plot_elf_p_trend(schedule)
