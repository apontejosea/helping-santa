# Overall toy schedule
plot_toy_schedule <- function(toys)  {
  p <- ggplot(toys) + 
    geom_segment(aes(x=start, xend=end, 
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

plot_p_recovery  <- function(n, work_per_day=5) {
  x     <- data.frame(p0=rep(0.25, n), p=NA)
  x$p[1]<- calc_p(x$p0[1], 10, 0)
  x$i <- 1:nrow(x)
  for(i in 2:nrow(x)) {
    x$p0[i]<- x$p[i-1]
    x$p[i] <- calc_p(x$p0[i], work_per_day, 0)
  }
  print(ggplot(x) + geom_line(aes(i, p)))
  x
}
