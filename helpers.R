#==============================================================
# Functions
#==============================================================
read_toys <- function(file) {
  toys             <- read.csv(file, as.is=T)
  toys$ArrivalTime <- strptime(toys$ArrivalTime, '%Y %m %d %H %M')
  toys$FinishTime  <- toys$ArrivalTime + (toys$Duration * 60)
  toys$ElfId       <- 0:(nrow(toys)-1) %% n_elves + 1
  return(toys)
}

read_solution <- function(solution_file_name) {
  yaml.load_file(solution_file_name)
}

# TODO: Still need to calculate elf productivity & start/end time of each toy
build_schedule <- function(s, toys) {
  res <- NULL
  for(i in seq_along(s)) {
    temp <- data.frame(ElfId = rep(names(s[i]), length(s[[i]])),
                       ToyId = s[[i]],
                       ToyDuration = toys$Duration[toys$ToyId %in% s[[i]]],
                       Arrival = toys$ArrivalTime[toys$ToyId %in% s[[i]]])
    # temp$start            <- calc_start_time(temp)
    # temp$elf_productivity <- calc_elf_productivity(temp)
    res  <- rbind(temp, res)
  }
  res$Start <- NULL
  for(i in seq_len(nrow(res))) {

  }
  res[order(-as.numeric(res$ElfId), res$ToyId, decreasing=F),]
}
