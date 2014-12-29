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

build_schedule_c <- function(toys, threshold=0, .parallel=FALSE) {
  require(plyr)
  cl  <- createCluster(lib=list('Rcpp'))
  toys[, Month:=as.numeric(strftime(Arrival, '%m'))]
  setorder(toys, ElfId, Month, Duration)
  toys <- subset(toys, T, -Month)
  res <- ddply(toys, .(ElfId), 
               .fun=function(X) { 
                 return(data.frame(ToyId=X$ToyId, 
                                   BookElf(X$Arrival, X$Duration, threshold=threshold)))
               }, 
               .parallel=.parallel)
  stopCluster(cl)
  setorder(toys, ToyId)
  res <- cbind(toys, res[order(res$ToyId), -c(1,2)])
  res
}

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
