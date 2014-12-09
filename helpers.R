library(yaml)
library(plyr)
library(ggplot2)
library(lubridate)
library(data.table)
# library(doSNOW)


#==============================================================
# Environment Options
#==============================================================

Sys.setenv(TZ="UTC")
# registerDoSNOW(makeCluster(4, type = "SOCK"))

#==============================================================
# Functions
#==============================================================

read_solution <- function(solution_file_name) {
  yaml.load_file(solution_file_name)
}

# TODO: This function will write the scheduling solutions to disk
write_solution <- function(sol) {
      
}

read_toys <- function(file, ...) {
  toys <- fread(file, colClasses=c('integer', 'myDate', 'integer'), ...)
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
  len            <- length(Arrival)
  res            <- data.table(p=numeric(len),
                               start=.POSIXct(numeric(len)),
                               end=.POSIXct(numeric(len)),
                               n=numeric(len),
                               m=numeric(len))
  res[1, p:=1]
  res[1, start:=max(date_9am(Arrival[1]), Arrival[1])]
  res[1, end:=start + Duration[1]/p[1]*60]
  res[1, n:=calc_sanctioned_hours(start, Duration[1])]
  res[1, m:=calc_unsanctioned_hours(start, Duration[1])]
  for(i in 2:nrow(res)) {
    res[i, p:=calc_p(res[i-1, p], res[i-1, n], res[i-1, m])]
    res[i, start:=
        as.POSIXct(max(c((date_9am(Arrival[i]) + res[i-1, m]), 
                         Arrival[i], res[i-1, end])))]
    res[i, end:=start + Duration[i]/p*60]
    res[i, n:=calc_sanctioned_hours(start, Duration[i])]
    res[i, m:=calc_unsanctioned_hours(start, Duration[i])]
  }
  res
}

book_elf2 <- function(Arrival, Duration) {
  len            <- length(Arrival)
  res            <- data.table(p=numeric(len),
                               start=.POSIXct(numeric(len)),
                               end=.POSIXct(numeric(len)),
                               n=numeric(len),
                               m=numeric(len))
  res$p[1]       <- 1
  res$start[1]   <- max(date_9am(Arrival[1]), Arrival[1])
  res$end[1]     <- with(res[1,], start + Duration/p*60)
  res$n[1]       <- calc_sanctioned_hours(start, Duration[1])
  res$m[1]       <- calc_unsanctioned_hours(start, Duration[1])
  for(i in 2:nrow(res)) {
    res$p[i]    <- with(res[i-1,], calc_p(p, n, m))
    res$start[i]<- with(res, max(c((date_9am(Arrival[i]) + m[i-1]), Arrival[i], end[i-1])))
    res$end[i]  <- res$start[i] + res$Duration[i]/res$p[i]*60
    res$n[i]    <- calc_sanctioned_hours(res$start[i], Duration[i])
    res$m[i]    <- calc_unsanctioned_hours(res$start[i], Duration[i])
  }
  res
}

book_elf3 <- function(Arrival, Duration) {
  len         <- length(Arrival)
  res         <- data.frame(
                    p=numeric(len),
                    start=.POSIXct(numeric(len)),
                    end=.POSIXct(numeric(len)),
                    n=numeric(len),
                    m=numeric(len))
  res$p[1]    <- 1
  res$start[1]<- max(date_9am(Arrival[1]), Arrival[1])
  res$end[1]  <- res$start[1] + Duration[1]/res$p[1]*60
  res$n[1]    <- calc_sanctioned_hours(res$start[1], Duration[1])
  res$m[1]    <- calc_unsanctioned_hours(res$start[1], Duration[1])
  for(i in 2:nrow(res)) {
    res$p[i]    <- calc_p(res$p[i-1], 
                          res$n[i-1], res$m[i-1])
    res$start[i]<- max(c((date_9am(Arrival[i]) + 
                          res$m[i-1]), Arrival[i], 
                         res$end[i-1]))
    res$end[i]  <- res$start[i] + Duration[i]/res$p[i]*60
    res$n[i]    <- calc_sanctioned_hours(res$start[i], Duration[i])
    res$m[i]    <- calc_unsanctioned_hours(res$start[i], Duration[i])
  }
  res
}

build_schedule <- function(toys) {
  toys[,c('p','start','end','n','m'):=
       book_elf(Arrival, Duration), by=ElfId]
  setorder(toys, ElfId, start)
  toys
}

build_schedule2 <- function(toys) {
  res <- ddply(toys, .(ElfId),
               function(X) book_elf3(X$Arrival, X$Duration))
  # res <- ddply(toys, .(ElfId),
  #              function(X) book_elf3(X$Arrival, X$Duration), .parallel=T)
  res <- cbind(toys, res[,-1])
  # res[order(res$ElfId, res$start),]
  res
}
