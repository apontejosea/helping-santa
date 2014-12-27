library(plyr)
library(lubridate)
library(data.table)

#=======================================================
# Environment Options
#=======================================================
Sys.setenv(TZ="UTC")

#=======================================================
# Functions
#=======================================================
read_solution <- function(solution_file_name) {
  require(yaml)
  yaml.load_file(solution_file_name)
}

# TODO: Still untested
write_solution <- function(schedule, file) {
  write.csv(schedule, file=file, row.name=FALSE)
}

read_toys <- function(file, ...) {
  require(data.table)
  require(lubridate)
  toys <- fread(file, colClasses=c('integer', 'myDate', 'integer'), ...)
  setnames(toys, names(toys)[2], 'Arrival')
  toys[, ':='(Arrival=parse_date_time2(Arrival, "%Y %m %d %H %M"))]
  # setorder(toys, Duration)
  return(toys)
}

distribute_toys <- function(n_elves, toys) {
  toys[ , ':='(ElfId=(0:(nrow(toys)-1) %% n_elves + 1))]
}

createCluster  <-  function(logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  require(plyr)
  num_cores <- parallel::detectCores()
  cl        <- makeCluster(num_cores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) {
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}

build_schedule_4 <- function(toys, threshold=0, .parallel=FALSE) {
  require(plyr)
  cl  <- createCluster(lib=list('Rcpp'))
  res <- ddply(toys, .(ElfId), 
               .fun=function(X) { 
                 BookElf4(X$Arrival, X$Duration, threshold=threshold) 
               }, 
               .parallel=.parallel)
  stopCluster(cl)
  res <- cbind(toys, res[,-1])
  res[order(res$ElfId, res$start),]
  res
}

build_schedule_c <- function(toys, threshold=0, .parallel=FALSE) {
  require(plyr)
  cl  <- createCluster(lib=list('Rcpp'))
  res <- ddply(toys, .(ElfId), 
               .fun=function(X) { 
                 BookElf5(X$Arrival, X$Duration, threshold=threshold) 
               }, 
               .parallel=.parallel)
  stopCluster(cl)
  res <- cbind(toys, res[,-1])
  res[order(res$ElfId, res$start),]
  res
}


# t = day time in hours (i.e. 7.5 for 7:30 AM, 13.75 for 1:45 PM)
# N <- function(t) {
#   stopifnot(t >= 0)
#   if(0  <= t & t < 9)   res <- 0
#   if(9  <= t & t < 19)  res <- t-9
#   if(19 <= t & t <= 24) res <- 10
#   res
# }

# t = day time in hours (i.e. 7.5 for 7:30 AM, 13.75 for 1:45 PM)
# M <- function(t) {
#   stopifnot(t >= 0)
#   if(0  <= t & t < 9)   res <- t
#   if(9  <= t & t < 19)  res <- 9
#   if(19 <= t & t <= 24) res <- t-10
#   res
# }

# EndOfWorkDay <- function(date_time) {
#   trunc(date_time, 'day') + 19*60*60
# }
# 
# BegOfWorkDay <- function(date_time) {
#   trunc(date_time, 'day') + 9*60*60
# }
# 
# 
# n_hours <- function(date_time) {
#   (as.numeric(date_time)-as.numeric(trunc(date_time, 'day')))/60/60
# }

# diff_days <- function(start, end) {
#   (as.numeric(trunc(end, 'day')) - as.numeric(trunc(start, 'day')))/60/60/24
# }
# 
# calc_sanctioned_hours <- function(start, duration) {
#   expected_end <- start + duration*60
#   N(n_hours(expected_end)) - N(n_hours(start)) + diff_days(start, expected_end)*10
# }
# 
# calc_unsanctioned_hours <- function(start, duration) {
#   expected_end <- start + duration*60
#   M(n_hours(expected_end)) - M(n_hours(start)) + diff_days(start, expected_end)*14
# }

# calc_p    <- function(p0, n, m) { 
#   min(4, max(0.25, p0*(1.02)^n*(0.9)^m)) 
# }
# 
# date_9am  <- function(dtime) { 
#   trunc(dtime,'day')+9*60^2 
# }
# 
# next_9am  <- function(dtime) {
#   my_date    <- trunc(dtime, 'day')
#   addition   <- 9*60*60
#   if(n_hours(dtime) > 9) {
#     addition <- addition + 24*60*60
#   }
#   my_date + addition
# }

# look ahead in earliest and alternative productivity to choose when to start building the toy
# pick_start <- function(earliest_start, duration, productivity, threshold) {
#   s_e <- earliest_start
#   s_a <- max(s_e, next_9am(s_e))
#   n_e <- calc_sanctioned_hours(s_e, duration)
#   n_a <- calc_sanctioned_hours(s_a, duration)
#   m_e <- calc_unsanctioned_hours(s_e, duration)
#   m_a <- calc_unsanctioned_hours(s_a, duration)
#   p_e <- calc_p(productivity, n_e, m_e)
#   p_a <- calc_p(productivity, n_a, m_a)
#   advantage_index <- (p_a - p_e)*(s_a - s_e)
#   if(advantage_index > threshold)  { s_a }
#   else  { s_e }
# }


# This function adds several columns corresponding to booking elf's
# schedule, considering time constraints & productivity:
#  start: starting time
#  end: ending time
#  n: sanctioned hours
#  m: unsanctioned hours
#  productivity: p = p' * (1.02)^n * (0.9)^m
# working on bad hours relentlessly
# not respecting penalty of previous toy
# book_elf3 <- function(Arrival, Duration, threshold) {
#   len         <- length(Arrival)
#   res         <- data.frame(
#                     p=numeric(len),
#                     start=.POSIXct(numeric(len)),
#                     end=.POSIXct(numeric(len)),
#                     n=numeric(len),
#                     m=numeric(len))
#   res$p[1]        <- 1
#   earliest_start  <- max(date_9am(Arrival[1]), Arrival[1])
#   res$start[1]    <- pick_start(earliest_start, Duration[1], res$p[1], 
#                                 threshold)
#   res$end[1]      <- res$start[1] + Duration[1]/res$p[1]*60
#   res$n[1]        <- calc_sanctioned_hours(res$start[1], Duration[1])
#   res$m[1]        <- calc_unsanctioned_hours(res$start[1], Duration[1])
#   for(i in 2:nrow(res)) {
#     res$p[i]      <- calc_p(res$p[i-1], res$n[i-1], res$m[i-1])
#     earliest_start<- res$end[i-1]+res$m[i-1]
#     res$start[i]  <- pick_start(earliest_start, Duration[i], 
#                                 res$p[i], threshold)
#     res$end[i]    <- res$start[i] + Duration[i]/res$p[i]*60
#     res$n[i]      <- calc_sanctioned_hours(res$start[i], Duration[i])
#     res$m[i]      <- calc_unsanctioned_hours(res$start[i], 
#                                              Duration[i])
#   }
#   res
# }


# build_schedule_c <- function(toys) {
#   require(plyr)
#   res <- ddply(toys, .(ElfId),
#                function(X) book_elf3(X$Arrival, X$Duration, threshold=0))
#   res <- cbind(toys, res[,-1])
#   res[order(res$ElfId, res$start),]
#   res
# }
