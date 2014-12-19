########################################################
##  To run, just source this file in R:
##    source('allocate_elves.R')
########################################################
library(elves)
options(width=600)
Sys.setenv(TZ="UTC")
#=======================================================
# Input
#=======================================================
n_elves            <- 900
n_toys             <- 10000000
toys_file_name     <- 'data/toys_rev2.csv'
# toys_file_name     <- 'test_toys_small.csv'

#=======================================================
# Steps
#=======================================================
print(system.time(toys       <- read_toys(toys_file_name, nrows=(n_toys+1))))
print(system.time(sel_toys   <- distribute_toys(n_elves, toys)))
print(system.time(schedule_c <- build_schedule_c(sel_toys, .parallel=T)))
# print(system.time(schedule_4 <- build_schedule_4(sel_toys, .parallel=T)))


submission_data <- subset(schedule_c,T,c(ToyId, ElfId, start, end))
submission_data$Duration <- ceiling(as.numeric(submission_data$end - submission_data$start)/60)
submission_data <- subset(submission_data,T,-end)
setnames(submission_data, c('ToyId','ElfId','StartTime','Duration'))
submission_data[,StartTime:=strftime(StartTime, '%Y %m %d %H %M')]

write.csv(submission_data, file='our_submission.csv', row.names=F)
# print(schedule_c)
# print(schedule_4)
last_subset <- 50 
# Uncomment to generate graphs
PngIt(plot_duration(tail(schedule_c, last_subset)), 
      'toy_duration_c.png')
PngIt(plot_toy_schedule(tail(schedule_c, last_subset)), 
      'overall_toy_schedule_c.png')
PngIt(plot_elf_schedule(tail(schedule_c, last_subset)), 
      'overall_elf_schedule_c.png')
PngIt(plot_elf_p_trend(tail(schedule_c, last_subset)), 
      'elf_p_trend_c.png')

PngIt(plot_duration(tail(schedule_4, last_subset)), 
      'toy_duration_4.png')
PngIt(plot_toy_schedule(tail(schedule_4, last_subset)), 
      'overall_toy_schedule_4.png')
PngIt(plot_elf_schedule(tail(schedule_4, last_subset)), 
      'overall_elf_schedule_4.png')
PngIt(plot_elf_p_trend(tail(schedule_4, last_subset)), 
      'elf_p_trend_4.png')
# plot_duration(schedule_c)
# plot_toy_schedule(schedule_c)
# plot_elf_schedule(schedule_c)
# plot_elf_p_trend(schedule_c)
# 
# plot_duration(schedule_4)
# plot_toy_schedule(schedule_4)
# plot_elf_schedule(schedule_4)
# plot_elf_p_trend(schedule_4)
