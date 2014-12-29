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
# toys_file_name     <- 'data/toys_sample.csv'

#=======================================================
# Functions
#=======================================================
CreateSubmission <- function(schedule, file_name) {
    #duration is calculated on .cpp
    submission_data <- subset(schedule,T,c(ToyId, ElfId, start, end, duration))
    submission_data <- subset(submission_data,T,-end)
    setnames(submission_data, c('ToyId','ElfId','StartTime','Duration'))
    setorder(submission_data, StartTime)
    submission_data[, StartTime:=strftime(StartTime, '%Y %m %d %H %M')]
    write.csv(submission_data, file=file_name, row.names=F)
}

# GenerateSampleToysFile <- function(input_file, output_file) {
#   input <- read.csv(input_file, as.is=T)
#   res <- input[, c('ToyId', 'ArrivalTime', 'Duration')]
#   res$ArrivalTime <- strftime(res$ArrivalTime, '%Y %m %d %H %M')
#   names(res)  <- c('ToyId', 'Arrival_time', 'Duration')
#   write.csv(res, file=output_file, row.names=F)
# }

#=======================================================
# Steps
#=======================================================
print(system.time(toys       <- read_toys(toys_file_name, nrows=(n_toys+1))))

# S              <- toys$ToyId
# result         <- CalculateObjective(S, toys, n_elves)
# curr_objective <- result$objective
# schedule       <- result$schedule


# print(system.time(org_toys     <- distribute_toys(n_elves, toys)))
# org_toys<-org_toys[order(org_toys$ElfId,as.numeric(format(org_toys$Arrival,"%m")),org_toys$Duration)]
# print(system.time(org_schedule <- build_schedule_c(org_toys[ElfId==313,], .parallel=T)))

# Optimization
# GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys[order(toys$Duration),], n_elves=n_elves, min=1, 
#          max=max(toys$ToyId), popSize=10, maxiter = 1, run=50, pmutation = 0.3)

print(system.time(org_toys     <- distribute_toys(n_elves, toys[order(toys$Duration),])))
print(system.time(org_schedule <- build_schedule_c(org_toys, .parallel=T)))


# print(system.time(opt_toys     <- distribute_toys(n_elves, toys[order(as.numeric(GA@solution)),])))
# print(system.time(opt_schedule <- build_schedule_c(opt_toys, .parallel=T)))

rnd_ind <- sample(1:n_toys, size=n_toys, replace=F)
print(system.time(rnd_toys     <- distribute_toys(n_elves, toys[order(toys$Duration),])))
print(system.time(rnd_schedule <- build_schedule_c(rnd_toys, .parallel=T)))

CreateSubmission(org_schedule, 'org_sub.csv')
CreateSubmission(rnd_schedule, 'rnd_sub.csv')
# CreateSubmission(opt_schedule, 'opt_sub.csv')

# PngIt(plot_duration(opt_schedule), 'toy_duration_opt.png')
# PngIt(plot_toy_schedule(opt_schedule), 'overall_toy_schedule_opt.png')
# PngIt(plot_elf_schedule(opt_schedule), 'overall_elf_schedule_opt.png')
# PngIt(plot_elf_p_trend(opt_schedule), 'elf_p_trend_opt.png')

# PngIt(plot_duration(org_schedule), 'toy_duration_org.png')
# PngIt(plot_toy_schedule(org_schedule), 'overall_toy_schedule_org.png')
# PngIt(plot_elf_schedule(org_schedule), 'overall_elf_schedule_org.png')
# PngIt(plot_elf_p_trend(org_schedule), 'elf_p_trend_org.png')

# PngIt(plot_duration(schedule), 'test_toy_duration.png')
# PngIt(plot_toy_schedule(schedule), 'test_overall_toy_schedule.png')
# PngIt(plot_elf_schedule(schedule), 'test_overall_elf_schedule.png')
# PngIt(plot_elf_p_trend(schedule), 'test_elf_p_trend.png')
