require(ggplot2)
require(scales)
theme_set(theme_linedraw(12))

PdfIt <- function(graph, file) {
  mult <- 1.5
  graphics.off()
  png(file=file, width=11*mult, height=8.5*mult)
  print(graph)
  dev.off()
}

PngIt <- function(graph, file) {
  res <- 600
  mult <- 5.5
  graphics.off()
  png(file=file, width=800*mult, height=600*mult, res=res)
  print(graph)
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

# Duration graph
plot_duration <- function(schedule)  {
  require(ggplot2)

  ggplot(schedule) + 
    geom_point(aes(x=ToyId, y=Duration)) +
   ylab('Duration (hours)')
}


plot_combined <- function(schedule, last_date=NULL) {
  library(gtable)
  max_date <- ifelse(!is.null(last_date), last_date, max(schedule$end))
  dt_lims <- scale_x_datetime(breaks='1 day', 
                              limits = c(as.POSIXct('2014-01-01'), max_date))
  bind1 <- rbind(ggplotGrob(plot_toy_schedule(schedule)+dt_lims),
                 ggplotGrob(plot_elf_schedule(schedule)+dt_lims), size='last')
  grid.draw(bind1)
}

plot_elf_schedule <- function(schedule) {
  require(ggplot2)
  date_range <- range(trunc(schedule$start, 'day'))
  all_dates  <- seq(date_range[1], date_range[2], 'days')
  time_frames<- data.frame(start=all_dates+9*60*60, end=all_dates+19*60*60, 
                           bot=(min(schedule$ElfId)-0.5), top=(max(schedule$ElfId)+0.5))
  ggplot(schedule) +
    geom_rect(aes(xmin=start, xmax=end, ymin=bot, ymax=top), fill='blue', alpha=0.3, data=time_frames) +
    geom_segment(aes(x=start, xend=end, y=p, yend=p, color=as.factor(ToyId), size=1.5)) + 
    theme(panel.grid.major.x=element_line(size=1)) +
    scale_y_reverse() + scale_color_hue('accent') + scale_x_datetime(breaks='1 day') +
    facet_grid(ElfId~.) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")
}

plot_elf_p_trend  <- function(schedule) {
  require(ggplot2)
  date_range <- range(trunc(schedule$start, 'day'))
  all_dates  <- seq(date_range[1], date_range[2], 'days')
  time_frames<- data.frame(start=all_dates+9*60*60, end=all_dates+19*60*60,
                           bot=(min(schedule$ElfId)-1), top=(max(schedule$ElfId)+1))
  ggplot(schedule) +
    geom_rect(aes(xmin=start, xmax=end, ymin=bot, ymax=top), fill='blue', alpha=0.8, data=time_frames) +
    geom_segment(aes(x=start, xend=end, y=p, yend=p, color=as.factor(ToyId)), size=1) +
    theme(panel.grid.major.x=element_line(size=1)) +
    scale_color_hue('accent') +
    facet_grid(ElfId~.) + scale_color_hue('accent') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")
}

# Overall toy schedule
plot_toy_schedule <- function(schedule) {
  require(ggplot2)
  date_range <- range(trunc(schedule$start, 'day'))
  all_dates  <- seq(date_range[1], date_range[2], 'days')
  time_frames<- data.frame(start=all_dates+9*60*60, end=all_dates+19*60*60,
                           bot=(min(schedule$ToyId)-1), top=(max(schedule$ToyId)+1))
  ggplot(schedule) +
    geom_rect(aes(xmin=start, xmax=end, ymin=bot, ymax=top), fill='blue', alpha=0.3, data=time_frames) +
    geom_segment(aes(x=Arrival, xend=Arrival+Duration*60, y=ToyId, yend=ToyId, color=as.factor(ElfId), size=1.5)) +
    theme(panel.grid.major.x=element_line(size=1)) +
    scale_y_reverse() + scale_color_hue('accent') + scale_x_datetime(breaks='1 day') +
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.position="none")
}
