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
