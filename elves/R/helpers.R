# library(plyr)
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
  setorder(toys, ToyId)
  return(toys)
}

distribute_toys <- function(n_elves, toys) {
  toys[ , ':='(ElfId=(0:(nrow(toys)-1) %% n_elves + 1))]
  toys
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


CalculateMaxSanctionedDuration <- function(previous_end) {
  next_sanctioned_time <- NextSanctionedDateTime(previous_end)

  (as.numeric(DateTimeAt1900(next_sanctioned_time)) - 
   as.numeric(next_sanctioned_time))/60
}

PickStart <- function(previous_start, previous_end, duration, productivity) {

  previous_start_1900 = DateTimeAt1900(previous_start)
  
  rest_time_hr = max(0.0, (previous_end - previous_start_1900)/60/60)
  s_e          = CalculateDateTimeAfterResting(previous_end, rest_time_hr)+60

  if(CalculateUnsanctionedHours(s_e , duration) > 0) {
    s_e          = DateTimeNext900(s_e);
  }

  return(s_e)
}

# This function assumes toys are sorted by duration
SelectNextToyId <- function(p_prev, curr_time, toys, max_sanct_duration,
                            p_med = 2, p_high = 3.5) {

  next_toyid   <- NA
  min_duration <- min(toys[Unscheduled == TRUE & Arrival <= curr_time, Duration])

  if(p_prev < p_med) {
    # Select smallest available toy
    min_duration <- min(toys[Unscheduled == TRUE & Arrival <= curr_time, 
                        Duration])
    next_toyid   <- toys[Unscheduled == TRUE & Arrival <= curr_time & Duration==min_duration, 
                         ToyId][1]
  }
  else if(p_med <= p_prev & p_prev <= p_high) {
    # Select largest available toy that better fits in this duration slot
    next_toyid <- toys[Unscheduled == TRUE & Arrival <= curr_time & Duration<=max_sanct_duration*p_prev, 
                       ToyId][1]
  }
  else {
    # Select second largest available toy
    next_toyid <- last(toys[Unscheduled == TRUE & Arrival <= curr_time,
                       ToyId])
  }

  # if(is.na(next_toyid) |
  #    is.na(toys[ToyId==next_toyid, Duration]) |
  #    is.na(p_prev)) {
  #   cat('\n')
  #   cat('Next ToyId    :  ', next_toyid, '\n')
  #   cat('Previous p    :  ', p_prev, '\n')
  #   cat('Toy Duration  :  ', toys[ToyId==next_toyid, Duration], '\n')
  #   cat('Elf Duration  :  ', toys[ToyId==next_toyid, Duration]/p_prev, '\n')
  #   stop('Error...')
  # }
  return(next_toyid)
}


BookElfOpt <- function(toys, p_med = 2, p_high = 4) {

  toys     <- as.data.table(toys)
  # Initialize
  p_prev   <- numeric(0)

  num_na   <- rep(as.numeric(NA), nrow(toys))
  bool_na  <- rep(as.logical(NA), nrow(toys))
  posix_na <- rep(as.POSIXct(NA), nrow(toys))
  schedule <- data.table(ToyId        = num_na,
                         Arrival      = posix_na,
                         p            = num_na,
                         start        = posix_na,
                         end          = posix_na,
                         ElfDuration  = num_na,
                         n            = num_na,
                         m            = num_na)

  # browser()
  # All toys are unscheduled at the beginning
  toys[, Unscheduled := TRUE]

# Insert first (shortest) toy in schedule
  schedule[1,ToyId      := toys$ToyId[1]]
  schedule[1,Arrival    := toys$Arrival[1]]
  # schedule[1,Duration   := toys$Duration[1]]
  schedule[1,p          := 1.0]
  schedule[1,ElfDuration:= CalculateElfDurationMinutes(toys$Duration[1], p)]
  schedule[1,start      := NextSanctionedDateTime(toys$Arrival[1])]
  schedule[1,end        := start + ElfDuration*60]
  schedule[1,n          := CalculateSanctionedHours(start, ElfDuration)]
  schedule[1,m          := CalculateUnsanctionedHours(start, ElfDuration)]

# For each or the n-1 toys remaining to schedule
#   max_duration: Determine max duration without penalty at this index
  for(i in 2:nrow(schedule)) {
    p_prev       <- schedule[i-1, p]
    end_prev     <- schedule[i-1, end]
    current_time <- schedule[i-1, end]+60

    max_sanct_duration     <- CalculateMaxSanctionedDuration(current_time)
    min_duration           <- min(toys[Unscheduled==TRUE & Arrival <= current_time, Duration])

    # Calculate schedule parameters
    schedule[i, p          := CalculateP(schedule[i-1, p], schedule[i-1, n], schedule[i-1, m])]
    earliest_start         <- PickStart(schedule[i-1, start], schedule[i-1, end], min_duration, 
                                        schedule[i, p])
    next_toyid             <- SelectNextToyId(p_prev, current_time, toys, max_sanct_duration, p_med = 2, p_high = 3.5)
    schedule[i, ToyId      := next_toyid]
    schedule[i, Arrival    := toys$Arrival[i]]
    # schedule[i, Duration   := toys$Duration[i]]
    schedule[i, start      := max(earliest_start, NextSanctionedDateTime(toys$Arrival[toys$ToyId==next_toyid]))]
    schedule[i, ElfDuration:= CalculateElfDurationMinutes(toys$Duration[toys$ToyId==next_toyid], p)]
    schedule[i, end        := start + ElfDuration*60]
    schedule[i, n          := CalculateSanctionedHours(start, toys$Duration[toys$ToyId==next_toyid])]
    schedule[i, m          := CalculateUnsanctionedHours(start, toys$Duration[toys$ToyId==next_toyid])]
    toys[ToyId == next_toyid, Unscheduled := FALSE]
  }

  # browser()
  setnames(schedule, 'ElfDuration', 'Duration')
  subset(schedule,T, c(ToyId,Arrival,p,start,end,n,m,Duration))
}


build_schedule_c <- function(schedule, threshold=0, .parallel=FALSE) {
  require(plyr)
  cl  <- createCluster(lib=list('Rcpp'))
  schedule[, Month:=as.numeric(strftime(Arrival, '%m'))]
  # setorder(schedule, ElfId, Month, Duration)
  schedule <- subset(schedule, T, -Month)
  res <- ddply(schedule, .(ElfId), 
               .fun=function(X) { 
                 return(data.frame(ToyId=X$ToyId, 
                                   BookElf(X$Arrival, X$Duration, threshold=threshold)))
               }, 
               .parallel=.parallel)
  
  stopCluster(cl)
  setorder(schedule, ToyId)
  res <- cbind(schedule, res[order(res$ToyId), -c(1,2)])
  res
}

build_schedule_opt2 <- function(schedule, .parallel=FALSE) {
  require(foreach)
  if(.parallel) {
    cl  <- createCluster(lib=list('Rcpp', 'data.table', 'elves'))
  }
  # browser()
  res <- foreach(elf=unique(schedule$ElfId), 
    .combine=rbind,
    .packages=c('elves', 'Rcpp', 'data.table')) %dopar% {
       data.frame(ElfId=elf, BookElfOpt(schedule[ElfId==elf], p_med = 2, p_high = 4))
  }
  if(.parallel) {
    stopCluster(cl)
  }
  # setorder(schedule, ToyId)
  # res <- cbind(schedule, res[order(res$ToyId), -c(1,2)])
  res
}

#build_schedule_opt <- function(schedule, .parallel=FALSE) {
#  require(plyr)
#  if(.parallel) {
#    cl  <- createCluster(lib=list('Rcpp', 'data.table'))
#  }
#  # browser()
#  res <- ddply(schedule, .(ElfId),
#               .fun=function(X) { 
#                 return(as.data.frame(BookElfOpt(X, p_med = 2, p_high = 4)))
#               }, 
#               .parallel=.parallel)
#  
#  if(.parallel) {
#    stopCluster(cl)
#  }
#  # setorder(schedule, ToyId)
#  # res <- cbind(schedule, res[order(res$ToyId), -c(1,2)])
#  res
#}

# S:       numeric vector with ordered ToyId's
# toys:    data.frame with columns ToyId, Arrival, Duration
# n_elves: numeric with number of elves to use
CalculateObjective <- function(S, toys, n_elves) {
  print(system.time(sel_toys   <- distribute_toys(n_elves, toys[S])))
  print(system.time(schedule_c <- build_schedule_c(sel_toys, .parallel=T)))
  tf      <- (as.numeric(max(schedule_c$end))-as.numeric(as.POSIXct('2014-01-01')))/60
  return(list(objective=tf*log(n_elves+1), schedule=schedule_c) )
}

CalculateFitness <- function(S, toys, n_elves) { 
  1/CalculateObjective(S, toys, n_elves)$objective 
}
