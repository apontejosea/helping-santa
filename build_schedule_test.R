########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################
library(GA)
library(elves)
options(width=600)
Sys.setenv(TZ="UTC")


#=======================================================
# Input
#=======================================================
n_elves            <- 900
n_toys             <- 10000000
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



#=======================================================
# Steps
#=======================================================
print(system.time(toys       <- read_toys(toys_file_name, nrows=(n_toys+1))))

# S              <- toys$ToyId
# result         <- CalculateObjective(S, toys, 4)
# curr_objective <- result$objective
# schedule       <- result$schedule
# CreateSubmission(schedule, 'our_sub.csv')

GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys, n_elves=n_elves, min=1, max=max(toys$ToyId), popSize=5, maxiter = 10, run=10, pmutation = 0.4)


print(system.time(opt_toys     <- distribute_toys(n_elves, toys[as.numeric(GA@solution)])))
opt_toys<-opt_toys[order(opt_toys$ElfId,as.numeric(format(opt_toys$Arrival,"%m")),opt_toys$Duration)]
print(system.time(opt_schedule <- build_schedule_c(opt_toys, .parallel=T)))

print(system.time(org_toys     <- distribute_toys(n_elves, toys)))
org_toys<-org_toys[order(org_toys$ElfId,as.numeric(format(org_toys$Arrival,"%m")),org_toys$Duration)]
print(system.time(org_schedule <- build_schedule_c(org_toys, .parallel=T)))

CreateSubmission(org_schedule, 'my_submission.csv')

# Uncomment to generate graphs
# last_subset <- 100
# PngIt(plot_duration(tail(schedule_c, last_subset)), 'toy_duration_c.png')
# PngIt(plot_toy_schedule(tail(schedule_c, last_subset)), 'overall_toy_schedule_c.png')
# PngIt(plot_elf_schedule(tail(schedule_c, last_subset)), 'overall_elf_schedule_c.png')
  # PngIt(plot_elf_p_trend(tail(schedule_c, last_subset)), 'elf_p_trend_c.png')

# PngIt(plot_duration(opt_schedule), 'toy_duration_opt.png')
# PngIt(plot_toy_schedule(opt_schedule), 'overall_toy_schedule_opt.png')
# PngIt(plot_elf_schedule(opt_schedule), 'overall_elf_schedule_opt.png')
# PngIt(plot_elf_p_trend(opt_schedule), 'elf_p_trend_opt.png')
# 
# PngIt(plot_duration(org_schedule), 'toy_duration_org.png')
# PngIt(plot_toy_schedule(org_schedule), 'overall_toy_schedule_org.png')
# PngIt(plot_elf_schedule(org_schedule), 'overall_elf_schedule_org.png')
# PngIt(plot_elf_p_trend(org_schedule), 'elf_p_trend_org.png')
