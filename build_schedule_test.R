########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################
library(elves)
library(GA)
options(width=600)
Sys.setenv(TZ="UTC")


#=======================================================
# Input
#=======================================================
n_elves            <- 10
n_toys             <- 100
toys_file_name     <- 'data/toys_rev2.csv'

#=======================================================
# Functions
#=======================================================
# TODO: Yara defined a way to sort and distribute toys more efficiently 
# per month and size.  Need to incorporate her contributions.

CreateSubmission <- function(schedule, file_name) {
  submission_data <- subset(schedule,T,c(ToyId, ElfId, start, end))
  submission_data$Duration <- ceiling(as.numeric(submission_data$end - submission_data$start)/60)
  submission_data <- subset(submission_data,T,-end)
  setnames(submission_data, c('ToyId','ElfId','StartTime','Duration'))
  submission_data[,StartTime:=strftime(StartTime, '%Y %m %d %H %M')]
  write.csv(submission_data, file=file_name, row.names=F)
}


# S:       numeric vector with ordered ToyId's
# n_elves: numeric with number of elves to use
CalculateObjective <- function(S, toys, n_elves) {
  print(system.time(sel_toys   <- distribute_toys(n_elves, toys[S])))
  print(system.time(schedule_c <- build_schedule_c(sel_toys, .parallel=T)))
  tf      <- (as.numeric(max(schedule_c$end))-as.numeric(as.POSIXct('2014-01-01')))/60
  return(list(objective=tf*log(n_elves+1), schedule=schedule_c) )
}

CalculateFitness <- function(S, toys, n_elves) { 
  1/CalculateObjective(S, toys, n_elves)$objective 
}

#=======================================================
# Steps
#=======================================================
print(system.time(toys       <- read_toys(toys_file_name, nrows=(n_toys+1))))

S              <- toys$ToyId
result         <- CalculateObjective(S, toys, 4)
curr_objective <- result$objective
schedule       <- result$schedule
CreateSubmission(schedule, 'our_sub.csv')

GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys, n_elves=10, min=1, max=max(toys$ToyId), popSize=50, maxiter = 5000, run=500, pmutation = 0.2)

print(system.time(opt_toys     <- distribute_toys(n_elves, toys[as.numeric(GA@solution)])))
print(system.time(org_toys     <- distribute_toys(n_elves, toys)))
print(system.time(opt_schedule <- build_schedule_c(opt_toys, .parallel=T)))
print(system.time(org_schedule <- build_schedule_c(org_toys, .parallel=T)))


# Uncomment to generate graphs
# last_subset <- 100
# PngIt(plot_duration(tail(schedule_c, last_subset)), 'toy_duration_c.png')
# PngIt(plot_toy_schedule(tail(schedule_c, last_subset)), 'overall_toy_schedule_c.png')
# PngIt(plot_elf_schedule(tail(schedule_c, last_subset)), 'overall_elf_schedule_c.png')
# PngIt(plot_elf_p_trend(tail(schedule_c, last_subset)), 'elf_p_trend_c.png')

PngIt(plot_duration(opt_schedule), 'toy_duration_opt.png')
PngIt(plot_toy_schedule(opt_schedule), 'overall_toy_schedule_opt.png')
PngIt(plot_elf_schedule(opt_schedule), 'overall_elf_schedule_opt.png')
PngIt(plot_elf_p_trend(opt_schedule), 'elf_p_trend_opt.png')

PngIt(plot_duration(org_schedule), 'toy_duration_org.png')
PngIt(plot_toy_schedule(org_schedule), 'overall_toy_schedule_org.png')
PngIt(plot_elf_schedule(org_schedule), 'overall_elf_schedule_org.png')
PngIt(plot_elf_p_trend(org_schedule), 'elf_p_trend_org.png')
