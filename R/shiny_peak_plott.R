#' @import behavr
#' @import data.table
#' @import ggplot2
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @import cowplot
#' @export

shiny_peak_plottR <- function(dt_curated, dt_peaks, gtype, ement, FRphase, filterHours = 16, phaseOffset = 0) {

  if (!"rhythmic" %in% colnames(metadata)) {
    metadata[, rhythmic := TRUE]
  }


  ggetho(data = dt_curated[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
                 aes(x = t, y = bpfiltered),
                 summary_time_window = mins(15)) +
    stat_ld_annotations(phase = hours(phaseOffset),
                        ld_colours = c("light yellow", "dark grey"),
                        alpha = 0.3, height = 1, outline = NA, ypos = "top") +
    geom_rect(data = metadata[genotype == gtype][entrainment == ement], aes(fill = rhythmic), xmin = -Inf,xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.3, inherit.aes = FALSE) +
    geom_line(data = dt_curated[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
              aes(y = activity),
              stat = "summary_bin", binwidth = 900, fun.y = "mean",
              colour = "red", size = 0.2) +
    geom_line(size = 0.75) +
    geom_point(data = dt_peaks[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
               aes(x = dt_curated[phase == FRphase][1, t] + peak*60, y = height)) +
    facet_wrap(. ~ uid, ncol = 8, scales = "free_y") +
    theme_minimal_hgrid(12) +
    ggtitle(paste(gtype, ement, FRphase, filterHours, "hours lowpass", sep = " "))

}
