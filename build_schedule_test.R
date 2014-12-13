########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################
library(elves7)

#=======================================================
# Input
#=======================================================
n_elves            <- 500
n_toys             <- 10000*n_elves
toys_file_name     <- 'data/toys_rev2.csv'

#=======================================================
# Steps
#=======================================================
system.time(toys       <- read_toys(toys_file_name, nrows=n_toys))
system.time(toys       <- distribute_toys(n_elves, toys))
system.time(schedule_c <- build_schedule_c(toys, .parallel=T))

# Uncomment to generate graphs
plot_duration(schedule_c)
plot_toy_schedule(schedule_c)
plot_elf_schedule(schedule_c)
plot_elf_p_trend(schedule_c)
