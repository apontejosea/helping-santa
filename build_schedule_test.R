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

# GenerateSampleToysFile <- function(input_file, output_file) {
#   input <- read.csv(input_file, as.is=T)
#   res <- input[, c('ToyId', 'ArrivalTime', 'Duration')]
#   res$ArrivalTime <- strftime(res$ArrivalTime, '%Y %m %d %H %M')
#   names(res)  <- c('ToyId', 'Arrival_time', 'Duration')
#   write.csv(res, file=output_file, row.names=F)
# }

#=======================================================
# Functions for new GA
#=======================================================
ComputeScore <- function(end_vector){
    ts = (as.numeric(max(end_vector)-as.numeric(as.POSIXct('2014-01-01')))/60)
    score2 = ts*log(1+n_elves)
    print(score2)
    return(score2)
}

#GA (Still not sure if it should be here or inside cpp)
#To Do: Save best schedule and Score
GA <- function(orig_schedule){
    #variable declarations
    current_iteration   = 1
    max_iteration       = 10
    current_score       = ComputeScore(orig_schedule$end)
    current_schedule<-orig_schedule[order(orig_schedule$ElfId)]
    duration_sum = sum(current_schedule$duration)
    print(duration_sum)
    
    #CreateSubmission(current_schedule, 'org_sub.csv')
    score_to_beat       = 1270117889 - (1270117889 * .10)
    
    while (current_score >= score_to_beat && current_iteration <= max_iteration){
        #save the best solution so far
        #best_schedule = current_schedule
        current_iteration        = current_iteration + 1
        current_mutated_schedule = data.table(MutationByP2(current_schedule$ElfId,current_schedule$ToyId,current_schedule$p,current_schedule$Duration,current_schedule$Arrival))
        current_schedule         = build_schedule_c(current_mutated_schedule, .parallel=T)
        current_schedule<-current_schedule[order(current_schedule$ElfId)]
        current_score            = ComputeScore(current_schedule$end)
        duration_sum = sum(current_schedule$duration)
        print(duration_sum)
    }
    return(current_score)
}

#=======================================================
# Steps for new GA
#=======================================================
#Get first solution
print(system.time(sel_toys   <- distribute_toys(n_elves, toys)))
#write.csv(sel_toys, file='toy_out.csv', row.names=F)
#Toys need to be sorted by ElfId so next sorting can work
sel_toys<-sel_toys[order(sel_toys$ElfId,as.numeric(format(sel_toys$Arrival,"%m")),sel_toys$Duration)]
sep_factor = 10
#sel_toys<-data.table(sort_toys(sel_toys$ElfId,sel_toys$ToyId,sel_toys$Duration,sel_toys$Arrival,sel_toys$BigToy,sep_factor))
sel_toys<-sort_toys(sel_toys, .parallel=T)
#write.csv(sel_toys, file='toy_out_sorted.csv', row.names=F)
#print(head(sel_toys))

#print(system.time(schedule_c <- build_schedule_c(sel_toys, .parallel=T)))
#current_score            = ComputeScore(schedule_c$end)
#print(current_score)

#Call GA
#print(system.time(GA(schedule_c)))
#GA(schedule_c)


#=======================================================
# Testing
#=======================================================
# S              <- toys$ToyId
# result         <- CalculateObjective(S, toys, n_elves)
# curr_objective <- result$objective
# schedule       <- result$schedule

#Get initial solution
#print(system.time(sel_toys   <- distribute_toys(n_elves, toys)))
#sel_toys<-sel_toys[order(sel_toys$ElfId,as.numeric(format(sel_toys$Arrival,"%m")),sel_toys$Duration)]
#sel_toys <- subset(sel_toys,T,-ElfId)


# Optimization
#suggestions = vector of solution string to be included in original population
#run = number of generations without improvement
#program runs aprox popSize*maxiter*2 mins

#GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys[order(as.numeric(format(toys$Arrival,"%m")),toys$Duration),], n_elves=n_elves, min=1,
#       max=max(toys$ToyId), popSize=10, maxiter = 1, run=1, pmutation = 0.5,pcrossover = 0.9, elitism = .2)


#GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys[order(as.numeric(format(toys$Arrival,"%m")),toys$Duration),], n_elves=n_elves, min=1,
#         max=max(toys$ToyId), popSize=10, maxiter = 1, run=1, pmutation = 0.5,pcrossover = 0.9, elitism = .2, suggestions = sel_toys$ToyId)
#GA <- ga(type = 'permutation', fitness = CalculateFitness, toys=toys, n_elves=n_elves, min=1,
#max=max(toys$ToyId), popSize=50, maxiter = 5, run=3, pmutation = 0.1,pcrossover = 0.7, elitism = .6, suggestions = sel_toys$ToyId)
#sel_toys$ElfId,as.numeric(format(sel_toys$Arrival,"%m")),sel_toys$Duration


#print(system.time(org_toys     <- distribute_toys(n_elves, toys[order(toys$Duration),])))
#print(system.time(org_schedule <- build_schedule_c(org_toys, .parallel=T)))

#print(system.time(opt_toys     <- distribute_toys(n_elves, toys[order(as.numeric(GA@solution)),])))
#print(system.time(opt_schedule <- build_schedule_c(opt_toys, .parallel=T)))

# rnd_ind <- sample(1:n_toys, size=n_toys, replace=F)
# print(system.time(rnd_toys     <- distribute_toys(n_elves, toys[rnd_ind,])))
# print(system.time(rnd_schedule <- build_schedule_c(rnd_toys, .parallel=T)))

# CreateSubmission(org_schedule, 'org_sub.csv')
# CreateSubmission(rnd_schedule, 'rnd_sub.csv')
#CreateSubmission(opt_schedule, 'opt_sub.csv')
#last_subset <- 3
# Uncomment to generate graphs
#PngIt(plot_elf_schedule(tail(opt_schedule, last_subset)),
#      'overall_elf_schedule_c.png')

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
