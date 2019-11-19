#' @import behavr
#' @export

curated_graph <- function(dt, dt_curated, na.rm = TRUE, ...){

    if (!dir.exists("./figures/curation/individual/")) {
    dir.create("./figures/curation/individual/", recursive = TRUE)
  }

  #create a list of unique groups i.e. treament groups
  treatment_list <- unique(attributes(dt)$metadata$treatment)

  #a loop to produce before and after curation graphs for comparison
  for (i in seq_along(treatment_list)) {
    plot_before <-
      ggetho(dt[xmv(treatment) == treatment_list[i]],
             aes(z = asleep)) +
      stat_ld_annotations(height = 1) +
      stat_tile_etho() +
      ggtitle(paste(treatment_list[i], "before curation (sleep)"))

    plot_after <-
      ggetho(dt_curated[xmv(treatment) == treatment_list[i]],
             aes(z = asleep)) +
      stat_ld_annotations(height = 1) +
      stat_tile_etho() +
      ggtitle(paste(treatment_list[i], "after curation (sleep)"))

    plot <- gridExtra::arrangeGrob(plot_before, plot_after, ncol = 2)

    ggsave(paste("./figures/curation/individual/", treatment_list[i], "_sleep_individual.pdf", sep = ""), plot = plot, width = 48, height = 27, units = "cm")
  }

  message("Figures output to /figures/curation/individual/")

}
