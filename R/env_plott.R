#' @import ggplot2
#' @import behavr
#' @import ggetho
#' @import data.table
#' @importFrom lubridate hour
#' @export

envPlottR <- function(start_datetime = NULL, stop_datetime = NULL, daysLD = NULL, phaseLD = NULL, plotError = FALSE, durationL = 12, ...) {

  if (!dir.exists("./figures/environmental/")) {
    dir.create("./figures/environmental/", recursive = TRUE)
  }

  filename <- file.choose()

  monitorID <- basename(tools::file_path_sans_ext(filename))

  ## read in raw data file

  data <- fread(filename, stringsAsFactors = FALSE)

  ## convert time in data file to POSIX datetime

  data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

  ## grab data columns and rename

  data[, light := rowMeans(data[, 12:15])]
  data[, temp := rowMeans(data[, 17:20])/10]
  data[, humidity := rowMeans(data[, 22:25])]

  if(plotError == TRUE) {

    data[, error := as.numeric(V4)]

   data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

  } else {

    data <- data[, c("time", "light", "temp", "humidity"), with = FALSE]

  }

  ## date, start and stop time and LD phase and LD days

  if(is.null(start_datetime)) {

    datestart <- min(data[, time])

  } else {

    datestart <- as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M:%S")

  }

  if(is.null(stop_datetime)) {

    datestop <- max(data[, time])

  } else {

    datestop <- as.POSIXct(stop_datetime, format = "%Y-%m-%d %H:%M:%S")

  }

  if(is.null(phaseLD)) {

    phaseLD <- hours(lubridate::hour(datestart))

  } else {

    phaseLD <- hours(phaseLD)

  }

  if(!is.null(daysLD)) {

    dateLD <- datestart + days(daysLD)

  }

  ## melt data

  moltendata <- melt(data, id = c('time'), variable.name = 'env.cond', value.name = 'value')

  if(exists("dateLD")) {

    env.plot <- ggplot(data = moltendata[time %between% c(datestart, datestop)], aes(x = time, y = value, colour = env.cond)) +
      stat_ld_annotations(phase = phaseLD, l_duration = hours(durationL), x_limits = c(datestart, dateLD),
                          ld_colours = c("yellow", "dark grey"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_ld_annotations(phase = phaseLD, l_duration = hours(durationL), x_limits = c(dateLD, datestop),
                          ld_colours = c("light grey", "dark grey"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      geom_line(size = 1) +
      scale_x_datetime(expand = c(0,0)) +
      facet_wrap( ~ env.cond, nrow = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank(),
            legend.position = "none")

  } else {

    env.plot <- ggplot(data = moltendata[time %between% c(datestart, datestop)], aes(x = time, y = value, colour = env.cond)) +
      stat_ld_annotations(phase = phaseLD, l_duration = hours(durationL), x_limits = c(datestart, datestop),
                          ld_colours = c("yellow", "dark grey"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      geom_line(size = 1) +
      scale_x_datetime(expand = c(0,0)) +
      facet_wrap( ~ env.cond, nrow = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank(),
            legend.position = "none")

  }

  print(env.plot)

  ggsave(filename = paste0("./figures/environmental/", monitorID, "_", datestart, "_", datestop, ".pdf"),
         height = 27,
         width = 48,
         units = "cm",
         plot = env.plot,
         device = "pdf")

  message("Figures output to /figures/environmental/")

 }



