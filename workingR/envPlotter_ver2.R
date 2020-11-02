#library(ggplot2)
#library(scales)
#library(ggetho)
#library(data.table)

filename <- file.choose()


## read in raw data file

data <- read.table(filename, stringsAsFactors = FALSE)

## date, start and stop time

datestart <- as.POSIXct(strptime('2018-03-18 08:00:00', format = "%Y-%m-%d %H:%M:%S"))
dateLD <- as.POSIXct(strptime('2018-03-21 08:00:00', format = "%Y-%m-%d %H:%M:%S"))
datestop <- as.POSIXct(strptime('2018-03-30 23:59:00', format = "%Y-%m-%d %H:%M:%S"))


## convert time in data file to POSIX datetime

time <- paste(data$V2,data$V3,data$V4,data$V5)
time <- as.POSIXct(strptime(time, format='%d %b %y %H:%M:%S'))

## grab data columns and rename

light <- data$V17
temp <- data$V22/10
humidity <- data$V27

## add all data to data.table

graphdata <- data.frame(time, light, temp, humidity, stringsAsFactors = FALSE)

## subset date between start and stop times

lims <- c(datestart, datestop)
graphdata <- subset(graphdata, time >= lims[1] & time <= lims[2])

## melt data

molten.data <- data.table::melt(graphdata, id = c('time'), variable.name = 'env.cond', value.name = 'value')

env.plot <- ggplot(data = molten.data, aes(x = time, y = value, colour = env.cond)) +
  stat_ld_annotations(phase = hours(hour(datestart)), x_limits = c(datestart, dateLD),
                      ld_colours = c("yellow", "dark grey"),
                      alpha = 0.2, height = 1, outline = NA, ypos = "top") +
  stat_ld_annotations(phase = hours(hour(datestart)), x_limits = c(dateLD, datestop),
                      ld_colours = c("light grey", "dark grey"),
                      alpha = 0.2, height = 1, outline = NA, ypos = "top") +
  geom_line() +
  scale_x_datetime(breaks = scales::date_breaks('1 day'), expand = c(0,0), limits = c(datestart, datestop)) +
  facet_wrap( ~ env.cond, nrow = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        legend.position = "none")



ggsave(filename = paste(tools::file_path_sans_ext(filename), datestart, "-", datestop, ".png", sep = "_"),
       plot = env.plot,
       device = "png")
