#' @import behavr
#' @import data.table
#' @export

idv_peak_graph <- function(dt_curated, smooth_window, FRphase, na.rm = TRUE, ...) {

  ## select FR data

  dt_FR <- dt_curated[phase == FRphase]

  ## unique treatment groups
  treatments <- unique(attributes(dt_FR)$metadata$treatment)

  ## unique fly ids
  ids <- unique(dt_FR$id)

  for (i in seq_along(treatments)) {

    if (!dir.exists(paste0("./figures/peak/individual/", treatments[i], "/"))) {
    dir.create(paste0("./figures/peak/individual/", treatments[i], "/"), recursive = TRUE)
    }
  }

  for (j in seq_along(ids)) {

    dt_idv <- dt_FR[id == ids[j]]
    dt_idv[, smth := TTR::SMA(activity, n = smooth_window), by = id]
    idv_treatment <- attributes(dt_idv)$metadata$treatment
    monitorID <- tools::file_path_sans_ext(unlist(strsplit(as.character(ids[j]), "|", fixed = TRUE))[2], compression = FALSE)
    flyID <- paste(idv_treatment, unlist(strsplit(as.character(ids[j]), "|", fixed = TRUE))[3], sep = "_")

    dt_idv[, autoco := scales::rescale(acf(smth, lag.max = 5760, plot = FALSE, na.action = na.pass)$acf, to = c(0,1)),
           by = id]

    act_peaks <- data.table(pracma::findpeaks(dt_idv$activity,
                                              minpeakdistance = 960,
                                              peakpat = "[+]{1,}[0]*[-]{1,}",
                                              npeaks = 4))

    setnames(act_peaks, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
    setorder(act_peaks, cols = "peak")

    act_plot <- ggetho(dt_idv,
                       #time_wrap = hours(24),
                       summary_FUN = mean,
                       summary_time_window = mins(1),
                       aes(y = activity)) +
      stat_ld_annotations(ld_colours = c("light grey", "dark grey"), alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_pop_etho() +
      geom_point(data = act_peaks, aes(x = dt_idv$t[1] + peak*60, y = height)) +
      theme(legend.position = "none") +
      ggtitle("Raw Activity")

    smt_peaks <- data.table(pracma::findpeaks(dt_idv$smth[smooth_window:length(dt_idv$smth)],
                                              minpeakdistance = 960,
                                              peakpat = "[+]{1,}[0]*[-]{1,}",
                                              npeaks = 4))

    setnames(smt_peaks, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
    smt_peaks$height <- round(smt_peaks$height, digits = 2)
    setorder(smt_peaks, cols = "peak")

    smt_plot <- ggetho(dt_idv,
                       #time_wrap = hours(24),
                       summary_FUN = mean,
                       summary_time_window = mins(1),
                       aes(y = smth)) +
      stat_ld_annotations(ld_colours = c("light grey", "dark grey"), alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_pop_etho() +
      geom_point(data = smt_peaks, aes(x = dt_idv$t[1] + peak*60, y = height)) +
      theme(legend.position = "none") +
      ggtitle(paste0(smooth_window, "min SMA Activity"))

    aut_peaks <- data.table(pracma::findpeaks(dt_idv$autoco,
                                              minpeakdistance = 960,
                                              peakpat = "{1,}[0]*[-]{1,}",
                                              npeaks = 4))

    setnames(aut_peaks, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
    aut_peaks$height <- round(aut_peaks$height, digits = 2)
    setorder(aut_peaks, cols = "peak")

        aut_plot <- ggetho(dt_idv,
                       #time_wrap = hours(24),
                       summary_FUN = mean,
                       summary_time_window = mins(1),
                       aes(y = autoco)) +
      stat_ld_annotations(ld_colours = c("light grey", "dark grey"), alpha = 0.2, height = 1, outline = NA, ypos = "top") +
      stat_pop_etho() +
      geom_point(data = aut_peaks, aes(x = dt_idv$t[1] + peak*60, y = height)) +
      theme(legend.position = "none") +
      ggtitle(paste0("Autocorrelation on ", smooth_window, "min SMA activity"))


      act_table <- gridExtra::tableGrob(act_peaks)
      smt_table <- gridExtra::tableGrob(smt_peaks)
      aut_table <- gridExtra::tableGrob(aut_peaks)

      plot <- gridExtra::arrangeGrob(act_plot,
                                     smt_plot,
                                     aut_plot,
                                     act_table,
                                     smt_table,
                                     aut_table,
                                     layout_matrix = rbind(c(1,2,3),
                                                           c(1,2,3),
                                                           c(4,5,6)),
                                     ncol = 3,
                                     nrow = 2,
                                     top = flyID)
    #plot <- gridExtra::grid.arrange(plot, aut_table, as.table = TRUE)

    ggsave(paste0("./figures/peak/individual/", idv_treatment, "/", monitorID, "_", flyID, "_", FRphase, "_", smooth_window, "-min_smooth_peaks.pdf"),
           plot = plot, width = 36, height = 20, units = "cm")

  }

  message("Figures output to /figures/peak/individual/")

}
