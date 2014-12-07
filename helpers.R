library(yaml)
library(plyr)
library(ggplot2)
library(fasttime)
library(lubridate)
library(data.table)

#==============================================================
# Environment Options
#==============================================================

Sys.setenv(TZ="UTC")

#==============================================================
# Functions
#==============================================================

read_solution <- function(solution_file_name) {
  yaml.load_file(solution_file_name)
}

# TODO: This function will write the scheduling solutions to disk
write_solution <- function(sol) {
      
}

read_toys <- function(file) {
  toys <- fread(file, colClasses=c('integer', 'myDate', 'integer'))
  setnames(toys, names(toys)[2], 'Arrival')
  toys[ , Arrival:=parse_date_time2(toys$Arrival, '%Y %m %d %H %M')]
  setorder(toys, Duration)
  return(toys)
}

distribute_toys <- function(n_elves, toys) {
  toys[ , ElfId:=(0:(nrow(toys)-1) %% n_elves + 1)]
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

calc_p <- function(p0, n, m) { min(4, max(0.25, p0*(1.02)^n*(0.9)^m)) }

date_9am <- function(dtime) { trunc(dtime,'day')+9*60^2 }

# This function adds several columns corresponding to booking elf's
# schedule, considering time constraints & productivity:
#  start: starting time
#  end: ending time
#  n: sanctioned hours
#  m: unsanctioned hours
#  productivity: p = p' * (1.02)^n * (0.9)^m
book_elf <- function(Arrival, Duration) {
  res       <- data.frame(p=rep(1, length(Arrival)))
  res$start <- max(date_9am(Arrival[1]), Arrival[1])
  res$end   <- with(res[1,], start + Duration/p*60)
  res$n     <- 0
  res$m     <- 0
  for(i in 2:nrow(res)) {
    res$p[i]    <- with(res[i-1,], calc_p(p, n, m))
    res$start[i]<- with(res, max(c((date_9am(Arrival[i]) + m[i-1]), Arrival[i], end[i-1])))
    res$end[i]  <- res$start[i] + res$Duration[i]/res$p[i]*60
    res$n[i]    <- calc_sanctioned_hours(res$start[i], Duration[i])
    res$m[i]    <- calc_unsanctioned_hours(res$start[i], Duration[i])
  }
  res
}

build_schedule <- function(toys) {
  toys[ , c('p','start','end','n','m'):=book_elf(Arrival, Duration), 
       by=ElfId]
  setorder(toys, ElfId, start)
  toys
}
