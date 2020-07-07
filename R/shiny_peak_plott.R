#' @import behavr
#' @import data.table
#' @import ggplot2
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @import cowplot
#' @export

shiny_peak_plottR <- function(dt_curated, dt_peaks, gtype, ement, FRphase, filterHours = 16, phaseOffset = 0) {

  # if (!paste0(FRphase, "_rhythmic") %in% colnames(metadata)) {
  #   metadata[, paste0(FRphase, "_rhythmic") := TRUE]
  # }


  ggetho(data = dt_curated[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
         aes(x = t, y = bpfiltered),
         summary_time_window = mins(15)) +
    stat_ld_annotations(phase = hours(phaseOffset),
                        ld_colours = c("light yellow", "dark grey"),
                        alpha = 0.3, height = 1, outline = NA, ypos = "top") +
    geom_rect(data = metadata[genotype == gtype][entrainment == ement], aes_string(fill = paste0(FRphase, "_rhythmic")), xmin = -Inf,xmax = Inf,
              ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
    scale_fill_manual(breaks = c(TRUE, FALSE), values = alpha(c("#2ca25f","#de2d26"), 0.2)) +
    geom_line(data = dt_curated[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
              aes(y = activity),
              stat = "summary_bin", binwidth = mins(120), fun.data = "mean_se",
              colour = "red", size = 0.2) +
    geom_line(size = 0.75) +
    geom_point(data = dt_peaks[phase == FRphase][xmv(genotype) == gtype][xmv(entrainment) == ement],
               aes(x = dt_curated[phase == FRphase][1, t] + peak*60, y = height), colour = "blue", size = 2.5) +
    facet_wrap(. ~ uid, ncol = 8, scales = "free_y") +
    theme_minimal_hgrid(12) +
    theme(axis.text.y = element_blank()) +
    ggtitle(paste(gtype, ement, FRphase, filterHours, "hours lowpass", sep = " "))

}
