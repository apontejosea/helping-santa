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
n_elves            <- 2
n_toys             <- 80 
toys_file_name     <- 'data/toys_small_sample.csv'
print(system.time(toys <- read_toys(toys_file_name, nrows=(n_toys+1))))

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

SelectWeightedRand <- function(val, weight=val) {
  stopifnot(length(val) == length(weight))
  totw <- sum(weight)
  frac <- cumsum(weight)/totw
  r    <- runif(1, 0, 1)
  val[frac > r][1]
}

#=======================================================
# Testing
#=======================================================
# S              <- toys$ToyId
# result         <- CalculateObjective(S, toys, n_elves)
# curr_objective <- result$objective
# schedule       <- result$schedule


# Optimization
Rprofmem('Rprofmem.out', threshold=0)
GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys[order(toys$Duration),], n_elves=n_elves, min=1, 
         max=max(toys$ToyId), popSize=10, maxiter = 2, run=10, pmutation = 0.3)
Rprofmem(NULL)
summaryRprof('Rprofmem.out')

print(system.time(org_toys     <- distribute_toys(n_elves, toys[order(toys$Duration),])))
print(system.time(org_schedule <- build_schedule_c(org_toys, .parallel=T)))

print(system.time(opt_toys     <- distribute_toys(n_elves, toys[order(as.numeric(GA@solution)),])))
print(system.time(opt_schedule <- build_schedule_c(opt_toys, .parallel=T)))

rnd_ind <- sample(1:n_toys, size=n_toys, replace=F)
print(system.time(rnd_toys     <- distribute_toys(n_elves, toys[rnd_ind,])))
print(system.time(rnd_schedule <- build_schedule_c(rnd_toys, .parallel=T)))

# CreateSubmission(org_schedule, 'org_sub.csv')
# CreateSubmission(rnd_schedule, 'rnd_sub.csv')
# CreateSubmission(opt_schedule, 'opt_sub.csv')

plot_elf_p_trend(opt_schedule)

plot_duration(org_schedule)
plot_elf_schedule(org_schedule)
plot_toy_schedule(org_schedule)
plot_elf_p_trend(org_schedule)

plot_duration(rnd_schedule)
plot_elf_schedule(rnd_schedule)
plot_elf_p_trend(rnd_schedule)


max_date <- max(max(opt_schedule$end),
                max(org_schedule$end),
                max(rnd_schedule$end))
PngIt(plot_combined(opt_schedule, max_date), 'combined_opt.png')
PngIt(plot_combined(org_schedule, max_date), 'combined_org.png')
PngIt(plot_combined(rnd_schedule, max_date), 'combined_rnd.png')

# PngIt(plot_duration(opt_schedule), 'toy_duration_opt.png')
# PngIt(plot_elf_schedule(opt_schedule), 'overall_elf_schedule_opt.png')
# PngIt(plot_elf_p_trend(opt_schedule), 'elf_p_trend_opt.png')
# PngIt(plot_toy_schedule(opt_schedule), 'overall_toy_schedule_opt.png')
# 
# PngIt(plot_duration(org_schedule), 'toy_duration_org.png')
# PngIt(plot_elf_schedule(org_schedule), 'overall_elf_schedule_org.png')
# PngIt(plot_elf_p_trend(org_schedule), 'elf_p_trend_org.png')
# PngIt(plot_toy_schedule(org_schedule), 'overall_toy_schedule_org.png')
# 
# PngIt(plot_duration(rnd_schedule), 'toy_duration_rnd.png')
# PngIt(plot_elf_schedule(rnd_schedule), 'overall_elf_schedule_rnd.png')
# PngIt(plot_elf_p_trend(rnd_schedule), 'elf_p_trend_rnd.png')
# PngIt(plot_toy_schedule(rnd_schedule), 'overall_toy_schedule_rnd.png')
