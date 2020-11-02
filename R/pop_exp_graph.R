#' @import behavr
#' @export

pop_exp_graph <- function(dt_curated, daysLD, smooth_window, na.rm = TRUE, ...) {

  if (!dir.exists("./figures/curation/population/")) {
    dir.create("./figures/curation/population/", recursive = TRUE)
  }

  #create a list of unique groups i.e. treament groups
  treatment_list <- unique(attributes(dt_curated)$metadata$treatment)

  #SMA for smoother plotting
  dt_curated[, smth := TTR::SMA(activity, n = smooth_window), by = id]

  #a loop to produce before and after curation graphs for comparison
  for (i in seq_along(treatment_list)) {
    plot_rawact <-
      ggetho(dt_curated[xmv(treatment) == treatment_list[i]],
             #time_wrap = hours(24),
             aes(x = t, y = activity, colour = entrainment),
             summary_time_window = mins(30)) +
      stat_ld_annotations(x_limits = c(0, days(daysLD)),
                          ld_colours = c("#ffffb3", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_ld_annotations(x_limits = c(days(daysLD), NA),
                          ld_colours = c("#bdbdbd", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_pop_etho() +
      facet_wrap(. ~ entrainment, nrow = 3) +
      theme(legend.position = "none") +
      ggtitle(paste(treatment_list[i], "population raw activity"))

    plot_smth30 <-
      ggetho(dt_curated[xmv(treatment) == treatment_list[i]],
             #time_wrap = hours(24),
             aes(x = t, y = smth, colour = entrainment),
             summary_time_window = mins(30)) +
      stat_ld_annotations(x_limits = c(0, days(daysLD)),
                          ld_colours = c("#ffffb3", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_ld_annotations(x_limits = c(days(daysLD), NA),
                          ld_colours = c("#bdbdbd", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +      stat_pop_etho() +
      facet_wrap(. ~ entrainment, nrow = 3) +
      theme(legend.position = "none") +
      ggtitle(paste(treatment_list[i], " population activity (", smooth_window, "min SMA)", sep = ""))

    plot_moving <-
      ggetho(dt_curated[xmv(treatment) == treatment_list[i]],
             #time_wrap = hours(24),
             aes(x = t, y = moving, colour = entrainment),
             summary_time_window = mins(30)) +
      stat_ld_annotations(x_limits = c(0, days(daysLD)),
                          ld_colours = c("#ffffb3", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_ld_annotations(x_limits = c(days(daysLD), NA),
                          ld_colours = c("#bdbdbd", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +      stat_pop_etho() +
      facet_wrap(. ~ entrainment, nrow = 3) +
      theme(legend.position = "none") +
      ggtitle(paste(treatment_list[i], "population activity (moving)"))

    plot_position <-
      ggetho(dt_curated[xmv(treatment) == treatment_list[i]],
             #time_wrap = hours(24),
             aes(x = t, y = Pn, colour = entrainment),
             summary_time_window = mins(30)) +
      stat_ld_annotations(x_limits = c(0, days(daysLD)),
                          ld_colours = c("#ffffb3", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_ld_annotations(x_limits = c(days(daysLD), NA),
                          ld_colours = c("#bdbdbd", "#636363"),
                          alpha = 0.2, height = 1, outline = NA, ypos = "top") +      stat_pop_etho() +
      facet_wrap(. ~ entrainment, nrow = 3) +
      theme(legend.position = "none") +
      ggtitle(paste(treatment_list[i], "population position"))

    plot <- gridExtra::arrangeGrob(plot_rawact, plot_smth30, plot_moving, plot_position, ncol = 2, nrow = 2)

    ggsave(paste("./figures/curation/population/", treatment_list[i], "_activty_population.pdf", sep = ""), plot = plot, width = 48, height = 27, units = "cm")
  }

  message("Figures output to /figures/curation/population/")

}
