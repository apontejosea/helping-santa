library(elves)
library(compare)
library(ggplot2)
library(data.table)
options(width=600)
Sys.setenv(TZ="UTC")
rm(list=ls())

if(interactive()) options(error=recover)

BookElfWrapper <- function(toys) {
  # Sort toys by duration
  # Book Elves Optimally
  # Sort toys by start
  setorder(toys, Duration)
  res <- data.table(BookElfOpt(toys))
  setorder(res, start)
}

#=======================================================
# Input
#=======================================================
n_elves                    <- 900
n_toys                     <- 10000000
# toys_file_name             <- 'data/toys_small_sample.csv'
toys_file_name             <- 'data/toys_rev2.csv'


#=======================================================
# Steps
#=======================================================
#   Read toys
toys <- read_toys(toys_file_name, nrows=n_toys)
setorder(toys, Duration)

schedule  <- distribute_toys(n_elves, toys)
setorder(schedule, ElfId, Duration)
# elf_schedule <- BookElfOpt(schedule[ToyId==1])

print(system.time(schedule_par   <-
  build_schedule_opt2(schedule, .parallel=T)))
# print(system.time(schedule_nopar <-
#   build_schedule_opt2(schedule, .parallel=F)))
# print(compare(schedule_par, schedule_nopar))


save.image()

# PngIt(plot_elf_p_trend(opt_schedule), 'p_trend_optimal.png')
