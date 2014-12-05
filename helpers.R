library(yaml)
library(ggplot2)

#==============================================================
# Functions
#==============================================================


read_toys <- function(file) {
  toys             <- read.csv(file, as.is=T)
  names(toys)      <- c('ToyId', 'Arrival', 'Duration')
  toys$Arrival     <- strptime(toys$Arrival, '%Y %m %d %H %M')
  toys$FinishTime  <- toys$Arrival + (toys$Duration * 60)
  toys$ElfId       <- 0:(nrow(toys)-1) %% n_elves + 1
  return(toys)
}

read_solution <- function(solution_file_name) {
  yaml.load_file(solution_file_name)
}

# TODO: This function will write the scheduling solutions to disk
write_solution <- function(sol) {
      
}


# Helper function that facilitates the indexing of toys_lineup
# using a modulus operation
elf_index <- function(toy_index, n_elves) {
  ((toy_index - 1) %% n_elves) + 1
}

# TODO: Very inefficient using lists.
# Maybe need to rewrite using data.table
distribute_toys <- function(n_elves, toys) {
  toys_lineup <- vector("list", n_elves)
  for(i in seq_len(nrow(toys))) {
    toys_lineup[[elf_index(i, n_elves)]]  <- 
      c(toys_lineup[[elf_index(i, n_elves)]], toys$ToyId[i])
  }
  toys_lineup
}

# t = day time in hours (i.e. 7.5 for 7:30 AM, 13.75 for 1:45 PM)
N <- function(t) {
  stopifnot(t >= 0)
  if(0  <= t & t < 9)   res <- 0
  if(9  <= t & t < 19)  res <- t-9
  if(19 <= t & t <= 24) res <- 10
  res
}

# t = day time in hours (i.e. 7.5 for 7:30 AM, 13.75 for 1:45 PM)
M <- function(t) {
  stopifnot(t >= 0)
  if(0  <= t & t < 9)   res <- t
  if(9  <= t & t < 19)  res <- 9
  if(19 <= t & t <= 24) res <- t-10
  res
}

n_hours <- function(date_time) {
  (as.numeric(date_time)-as.numeric(trunc(date_time, 'day')))/60/60
}

diff_days <- function(start, end) {
  (as.numeric(trunc(end, 'day')) - as.numeric(trunc(start, 'day')))/60/60/24
}

calc_sanctioned_hours <- function(start, duration) {
  expected_end <- start + duration*60
  N(n_hours(expected_end)) - N(n_hours(start)) + diff_days(start, expected_end)*10
}

calc_unsanctioned_hours <- function(start, duration) {
  expected_end <- start + duration*60
  M(n_hours(expected_end)) - M(n_hours(start)) + diff_days(start, expected_end)*14
}

# This function adds several columns corresponding to booking elf's
# schedule, considering time constraints & productivity:
#  start: starting time
#  end: ending time
#  n: sanctioned hours
#  m: unsanctioned hours
#  productivity: p = p' * (1.02)^n * (0.9)^m
book_elf <- function(elf_toy_lineup) {
  time_9am             <- trunc(elf_toy_lineup$Arrival,'day')+9*60^2
  elf_toy_lineup$p     <- 1
  elf_toy_lineup$start <- max(time_9am, elf_toy_lineup$Arrival)
  elf_toy_lineup$end   <- elf_toy_lineup$start + 
                            elf_toy_lineup$Duration/elf_toy_lineup$p*60
  elf_toy_lineup$n     <- 0
  elf_toy_lineup$m     <- 0
  for(i in 2:nrow(elf_toy_lineup)) {
    time_9am            <- trunc(elf_toy_lineup$Arrival[i],'day')+9*60^2
    elf_toy_lineup$p[i] <- with(elf_toy_lineup[i-1,], p*(1.02)^n*(0.9)^m)
    elf_toy_lineup$start[i]<- with(elf_toy_lineup,
                                max((time_9am + m[i-1]), Arrival[i]))
    elf_toy_lineup$end[i]<- elf_toy_lineup$start[i] + 
                           elf_toy_lineup$Duration[i]/elf_toy_lineup$p[i]*60
    elf_toy_lineup$n[i] <- with(elf_toy_lineup[i,], 
                             calc_sanctioned_hours(start, Duration))
    elf_toy_lineup$m[i] <- with(elf_toy_lineup[i,], 
                             calc_unsanctioned_hours(start, Duration))
  }
  elf_toy_lineup
}

build_schedule <- function(s, toys) {
  res <- NULL
  for(i in seq_along(s)) {
    # Current elf's toy lineup
    temp <- data.frame(
      ElfId   = rep(names(s[i]), length(s[[i]])),
      ToyId   = s[[i]],
      Duration = toys$Duration[toys$ToyId %in% s[[i]]],
      Arrival = toys$Arrival[toys$ToyId %in% s[[i]]])
    temp <- book_elf(temp)
    res  <- rbind(temp, res)
  }
  res[order(-as.numeric(res$ElfId), res$ToyId, decreasing=F),]
}

# Overall toy schedule
plot_toy_schedule <- function(toys)  {
  p <- ggplot(toys) + 
    geom_segment(aes(x=ArrivalTime, xend=FinishTime, 
                     y=ToyId, yend=ToyId)) + 
    scale_y_reverse()
  png(file='overall_toy_schedule.png', width=800, 
      height=600)
  print(p)
  dev.off()
}

# Duration graph
plot_duration <- function(toys)  {
  p <- ggplot(toys) + 
    geom_point(aes(x=ToyId, y=Duration)) +
   ylab('Duration (hours)')
  png(file='toy_duration.png', width=800, height=600)
  print(p)
  dev.off()
}
