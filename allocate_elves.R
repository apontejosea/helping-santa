library(yaml)
library(ggplot2)
source('helpers.R')

#==============================================================
# Input
#==============================================================
n_elves            <- 5
toys_file_name     <- 'toys_sample.csv'
solution_file_name <- 'initial_solution.yml'

#==============================================================
# Steps 
#==============================================================
toys     <- read_toys(toys_file_name)
s0       <- read_solution(solution_file_name)
schedule <- build_schedule(s0, toys)


# Overall toy schedule
p <- ggplot(toys) + 
  geom_segment(aes(x=ArrivalTime, xend=FinishTime, y=ToyId, yend=ToyId)) + 
  scale_y_reverse()
png(file='overall_toy_schedule.png', width=800, height=600)
print(p)
dev.off()

# Duration graph
p <- ggplot(toys) + 
  geom_point(aes(x=ToyId, y=Duration)) +
 ylab('Duration (hours)')
png(file='toy_duration.png', width=800, height=600)
print(p)
dev.off()
