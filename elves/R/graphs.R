require(ggplot2)
theme_set(theme_linedraw(12))

# Overall toy schedule
plot_toy_schedule <- function(toys)  {
  require(ggplot2)
  res <- 600
  mult <- 5.5
  p <- ggplot(toys) + 
    geom_segment(aes(x=start, xend=end, 
                     y=ToyId, yend=ToyId, color=ElfId)) + 
    scale_y_reverse()
  png(file='overall_toy_schedule.png', width=800*mult, height=600*mult, res=res)
  print(p)
  dev.off()
}

# Duration graph
plot_duration <- function(schedule)  {
  require(ggplot2)
  res <- 600
  mult <- 5.5
  p <- ggplot(schedule) + 
    geom_point(aes(x=ToyId, y=Duration)) +
   ylab('Duration (hours)')
  png(file='toy_duration.png', width=800*mult, height=600*mult, res=res)
  print(p)
  dev.off()
}

plot_p_recovery  <- function(n, work_per_day=5) {
  require(ggplot2)
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

plot_elf_schedule <- function(schedule) {
  require(ggplot2)
  res        <- 600
  mult       <- 5.5
  date_range <- range(trunc(schedule$start, 'day'))
  all_dates  <- seq(date_range[1], date_range[2], 'days')
  time_frames<- data.frame(start=all_dates+9*60*60, end=all_dates+19*60*60, bot=(min(schedule$ElfId)-1), top=(max(schedule$ElfId)+1))
  p <- ggplot(schedule) +
    geom_rect(aes(xmin=start, xmax=end, ymin=bot, ymax=top), fill='blue', alpha=0.3, data=time_frames) +
    geom_segment(aes(x=start, xend=end, y=ElfId, yend=ElfId, color=as.factor(ToyId), size=3)) + 
    theme(panel.grid.major.x=element_line(size=1)) +
    scale_y_reverse() + scale_color_hue('accent') + scale_x_datetime(breaks='1 day')
  png(file='overall_elf_schedule.png', width=800*mult, height=600*mult, res=res)
  print(p)
  dev.off()
}

plot_elf_p_trend  <- function(schedule) {
  require(ggplot2)  
  res  <- 600
  mult <- 5.5
  p <- ggplot(schedule) +
    geom_point(aes(x=start, y=p)) + facet_grid(ElfId~.) +
    scale_color_hue('accent')
  png(file='elf_p_trend.png', width=800*mult, height=600*mult, res=res)
  print(p)
  dev.off()
}
