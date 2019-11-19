#' @import behavr
#' @import ggetho
#' @import data.table
#' @export

pop_phase_graph <- function(dt_curated, exp_phase, param, bin_width, na.rm = TRUE, ...) {

  ## create output directory

    if (!dir.exists(paste0("./figures/phase/"))) {
    dir.create(paste0("./figures/phase/"), recursive = TRUE)
    }


  ## subset curated dt by selected phase
  dt <- dt_curated[phase == exp_phase]

  ## scaled activity?
  # if (param == "activity") {
  #   dt[, scaled := scales::rescale(activity, to = c(0,1)), by = id]
  #   param <- "scaled"
  # }

  ## select ld colours

  if (grepl("LD", exp_phase)) {
    LDcolours <- c("#ffffb3", "#636363")
  } else {
    LDcolours <- c("#bdbdbd", "#636363")
  }

  ## plot call

  plot_act <-   ggetho(dt,
                       summary_FUN = mean,
                       summary_time_window = mins(bin_width),
                       aes_string(y = param, colour = "entrainment")) +
    stat_ld_annotations(ld_colours = LDcolours, alpha = 0.2, height = 1, outline = NA, ypos = "top") +
    stat_pop_etho() +
    scale_fill_brewer(type = "seq", palette = "Dark2") +
    scale_colour_brewer(type = "seq", palette = "Dark2") +
    facet_wrap(temp ~ genotype, nrow = 2) +
    ggtitle(paste0(exp_phase, " ", param, " (", bin_width, "-min bin)"))

  plot_wrap <-   ggetho(dt,
                        time_wrap = hours(24),
                        summary_FUN = mean,
                        summary_time_window = mins(bin_width),
                        aes_string(y = param, colour = "entrainment")) +
    stat_ld_annotations(ld_colours = LDcolours, alpha = 0.2, height = 1, outline = NA, ypos = "top") +
    stat_pop_etho() +
    scale_fill_brewer(type = "seq", palette = "Dark2") +
    scale_colour_brewer(type = "seq", palette = "Dark2") +
    facet_wrap(temp ~ genotype, nrow = 1) +
    theme(legend.position = "none")

  plot <- gridExtra::arrangeGrob(plot_act, plot_wrap, ncol = 1, nrow = 2)

  ggsave(paste0("./figures/phase/", exp_phase, "_", param, "_", bin_width, "-min_bin.pdf"), plot = plot, width = 48, height = 27, units = "cm")

  message("Figures output to /figures/phase/")

}
