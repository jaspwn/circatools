#' @import behavr
#' @import data.table
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @export

peak_returnR <- function(dt_curated, filterHours = 16) {

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

  return(dt_peaks)
}