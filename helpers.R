#==============================================================
# Functions
#==============================================================
read_toys <- function(file) {
  toys             <- read.csv(file, as.is=T)
  toys$Arrival     <- strptime(toys$Arrival, 
                               '%Y %m %d %H %M')
  toys$FinishTime  <- toys$Arrival + 
                        (toys$Duration * 60)
  toys$ElfId       <- 0:(nrow(toys)-1) %% n_elves + 1
  return(toys)
}

read_solution <- function(solution_file_name) {
  yaml.load_file(solution_file_name)
}

# t = hours
N <- function(t) {
  stopifnot(t >= 0)
  if(0  <= t & t < 9)   res <- 0
  if(9  <= t & t < 19)  res <- t-9
  if(19 <= t & t <= 24) res <- 10
  res
}

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

calc_sanctioned_hours <- function(start=as.POSIXct('2014-01-01 04:00'), 
                                  duration=25*60) {
  expected_end <- start + duration*60
  print(paste('We start at:', start))
  print(paste('We end at:  ', expected_end))
  N(n_hours(expected_end)) - N(n_hours(start)) + diff_days(start, expected_end)*10
}

calc_unsanctioned_hours <- function(start, duration) {
  expected_end <- start + duration*60
  print(paste('We start at:', start))
  print(paste('We end at:  ', expected_end))
  M(n_hours(expected_end)) - M(n_hours(start)) + diff_days(start, expected_end)*14
}

# add several columns corresponding to booking elf's
# schedule, considering time constraints & productivity:
#  start: starting time
#  end: ending time
#  n: sanctioned hours
#  m: unsanctioned hours
#  productivity: p = p' * (1.02)^n * (0.9)^m
book_elf <- function(elf_toy_lineup) {
  elf_toy_lineup$p[1]     <- 1
  elf_toy_lineup$start[1] <- max((9am), elf_toy_lineup$Arrival)
  elf_toy_lineup$end[1]   <- start + elf_toy_lineup$Duration/p[1]
  elf_toy_lineup$n[1]     <- 0
  elf_toy_lineup$m[1]     <- 0
  for(t in 2:nrow(elf_toy_lineup)) {
    p[i]  = p[i-1] * (1.02)^n[i-1] * (0.9)^m[i-1]
    start = max((9am + m[i-1]), toy_arrival)
    end   = start + duration/p[i]
  }
}

# TODO: Still need to calculate elf productivity & start/end time of each toy
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
  res$Start <- NULL
  for(i in seq_len(nrow(res))) {

  }
  res[order(-as.numeric(res$ElfId), res$ToyId, decreasing=F),]
}
