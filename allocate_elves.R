library(ggplot2)

# Input
n_elves          <- 2
toys             <- read.csv('toys.csv', as.is=T)
toys$ArrivalTime <- strptime(toys$ArrivalTime, '%Y %m %d %H %M')
toys$FinishTime  <- toys$ArrivalTime + (toys$Duration * 60)
toys$Duration    <- with(toys, as.numeric(FinishTime) - as.numeric(ArrivalTime))/60
elves            <- data.frame(eid=1:n_elves, productivity=rep(1, n_elves), 
                               available=rep(TRUE, n_elves))


allocate_elves <- function(elves, toys) {
  production_line <- data.frame(elves=NULL, toys=NULL, 
                                duration=NULL, ending_time=NULL)

  finished_toys <- data.frame(toy_id=1, elf_id=2, 
            start_time=0, actual_duration=1)
  return(finished_toys)
}


# Overall toy schedule
p <- ggplot(toys) + geom_segment(aes(y=ToyId, yend=ToyId, x=ArrivalTime, xend=FinishTime)) + scale_y_reverse()
png(file='overall_toy_schedule.png', width=10, height=7.5)
print(p)
dev.off()

# Duration graph
p <- ggplot(toys) + geom_point(aes(x=ToyId, y=Duration)) + ylab('Duration (minutes)')
png(file='toy_duration.png', width=10, height=7.5)
print(p)
dev.off()


# available_elves(elves, allocation, toys, n)

# update available_elves
# select elf for next toy among available elves
# select_elf(elves, toy_duration)
# 
# 
# class elf
# 
# class toy
# 
# class production_line
# 
# 
# current_time <- 0
# 
# for(t in 1:nrow(toys)) {
#   
# }
