#' @import behavr
#' @import data.table
#' @import ggplot2
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @import cowplot
#' @export

peak_plottR <- function(dt_curated, treatment, FRphase = "FR1", filterHours = 16) {

  #low pass filter design to remove high frequency activity components
  bpfilt <- butter(n = 2, W = c((1/hours(filterHours))/((1/60)/2)), type = "low", plane = "z")

  #filter activity across entire experiment by individual
  dt_curated[, bpfiltered := as.vector(filtfilt(bpfilt, x = activity)),
             by = "id"]


  #find daily peak in activity for each fly and remap it to same metadata
  dt_peaks <- dt_curated[, data.table(findpeaks(bpfiltered,
                                                minpeakdistance = filterHours*60,
                                                peakpat = "{1,}[0]*[-]{1,}",
                                                npeaks = floor(length(t)/1440))),
                         by = c("id", "phase")]

  setnames(dt_peaks, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
  setorderv(dt_peaks, cols = c("id", "phase",  "peak"))
  setkeyv(dt_peaks, "id")
  setmeta(dt_peaks, metadata)
  dt_peaks[, .(id, uid) , meta = TRUE]
  dt_peaks[, uid := xmv(uid)]
  dt_curated[, uid := xmv(uid)]

  plot <- ggetho(data = dt_curated[phase == FRphase][xmv(treatment) == treatment],
                 aes(x = t, y = bpfiltered),
                 summary_time_window = mins(15)) +
    stat_ld_annotations(phase = hours(18),
                        ld_colours = c("light yellow", "dark grey"),
                        alpha = 0.3, height = 1, outline = NA, ypos = "top") +
    geom_line(data = dt_curated[phase == FRphase][xmv(treatment) == treatment],
              aes(y = activity),
              stat = "summary_bin", binwidth = 900, fun.y = "mean",
              colour = "red", size = 0.2) +
    geom_line(size = 0.75) +
    geom_point(data = dt_peaks[phase == FRphase][xmv(treatment) == treatment],
               aes(x = dt_curated[phase == FRphase][1, t] + peak*60, y = height)) +
    facet_wrap(. ~ uid, ncol = 8, scales = "free_y") +
    theme_minimal_hgrid(12) +
    ggtitle(paste(treatment, FRphase, sep = " "))

  print(plot)

  return(dt_peaks)

}
