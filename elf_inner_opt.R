library(elves)
library(ggplot2)
library(data.table)
options(width=600)
options(error=recover)
Sys.setenv(TZ="UTC")

rm(list=ls())

BookElfWrapper <- function(toys) {
  # Sort toys by duration
  # Book Elves Optimally
  # Sort toys by start
  setorder(toys, Duration)
  res <- data.table(BookElfOpt(toys, 0))
  setorder(res, start)
}

#=======================================================
# Input
#=======================================================
n_elves                    <- 8
n_toys                     <- 1000
# toys_file_name             <- 'data/toys_small_sample.csv'
toys_file_name             <- 'data/toys_rev2.csv'


#=======================================================
# Steps
#=======================================================
#   Read toys
toys <- read_toys(toys_file_name, nrows=n_toys)
# elf_schedule <- BookElfWrapper(schedule[ElfId==1])

#   Distribute toys (PENDING)
#     Distribute toys among elves
#   For each elf (PENDING... parallelized)
setorder(toys, Duration)
schedule     <- distribute_toys(n_elves, toys)
setorder(schedule, ElfId, Duration)

print(system.time(opt_schedule <- build_schedule_opt2(schedule, .parallel=T)))

save.image()

# PngIt(plot_elf_p_trend(opt_schedule), 'p_trend_optimal.png')
