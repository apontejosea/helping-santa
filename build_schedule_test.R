########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################
library(elves7)
options(width=600)
Sys.setenv(TZ="UTC")
#=======================================================
# Input
#=======================================================
n_elves            <- 5
n_toys             <- 100*n_elves
toys_file_name     <- 'data/toys_rev2.csv'

#=======================================================
# Steps
#=======================================================
system.time(toys       <- read_toys(toys_file_name, nrows=n_toys))
system.time(toys       <- distribute_toys(n_elves, toys))
system.time(schedule_c <- build_schedule_c(toys, .parallel=T))

library(Rcpp)
sourceCpp('test_master.cpp')
CalculateDaysDifference(as.POSIXct('2014-01-01 01:10'), 
                        as.POSIXct('2014-01-02'))


CalculateUnsanctionedHours(as.POSIXct('2014-01-01 09:10'), 300);



# Uncomment to generate graphs
plot_duration(schedule_c)
plot_toy_schedule(schedule_c)
plot_elf_schedule(schedule_c)
plot_elf_p_trend(schedule_c)
